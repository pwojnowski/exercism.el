;;; exercism-self-check.el --- Setup self-check report for exercism.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Interactive setup verification: sync CLI/config checks, async API
;; probes, and the `*exercism-self-check*' report buffer.

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'exercism-core)
(require 'exercism-cli)
(require 'exercism-api)
(require 'exercism-track-list)

(declare-function exercism--open-exercise-list "exercism-exercise-list")

(defvar exercism--self-check-results nil
  "Accumulator for `exercism-self-check' result lines.")

(defvar exercism--self-check-pending 0
  "Number of async checks still running in `exercism-self-check'.")

(defun exercism--masked-token (token)
  "Return TOKEN with the middle characters replaced by asterisks."
  (when token
    (let ((len (length token)))
      (if (< len 8)
          (make-string len ?*)
        (concat (substring token 0 4)
                (make-string (- len 8) ?*)
                (substring token -4))))))

(defun exercism--sync-self-check-results ()
  "Return a list of (LABEL OK-P DETAIL) for local setup checks."
  (let* ((exe (exercism--cli-executable-path))
         (user-config (exercism--read-user-config))
         (token (when user-config (alist-get 'token user-config)))
         (workspace (or (when user-config (alist-get 'workspace user-config))
                        exercism--workspace))
         (results
          (list
           (list "CLI executable" (not (null exe)) (or exe exercism-executable))
           (list "Config file"
                 (file-exists-p exercism-config-path)
                 exercism-config-path)
           (list "API token configured"
                 (and token (not (string-empty-p token)))
                 (if token
                     (exercism--masked-token token)
                   "missing from config")))))
    (if workspace
        (append results
                (list (list "Workspace directory"
                            (file-directory-p workspace)
                            workspace)))
      results)))

(defun exercism--setup-ok-p ()
  "Return non-nil when local Exercism setup passes sync checks."
  (not (seq-find (lambda (result) (not (nth 1 result)))
                 (exercism--sync-self-check-results))))

(defvar exercism-self-check-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'exercism-self-check-configure)
    (define-key map (kbd "g") #'exercism-self-check)
    (define-key map (kbd "t") #'exercism-self-check-select-track)
    (define-key map (kbd "e") #'exercism-self-check-open-exercises)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `exercism-self-check-mode'.")

(define-derived-mode exercism-self-check-mode special-mode "Exercism Self-Check"
  "Major mode for the Exercism setup self-check report."
  (setq buffer-read-only t))

(defun exercism--self-check-buffer ()
  "Return the `*exercism-self-check*' buffer, creating it if needed."
  (let ((buf (or (get-buffer "*exercism-self-check*")
                 (generate-new-buffer "*exercism-self-check*"))))
    (with-current-buffer buf
      (unless (derived-mode-p 'exercism-self-check-mode)
        (exercism-self-check-mode)))
    buf))

(defun exercism--self-check-line (label ok-p &optional detail)
  "Format one self-check result line for LABEL, OK-P, and optional DETAIL."
  (let ((mark (if ok-p
                  (propertize "✓" 'face 'success)
                (propertize "✗" 'face 'error))))
    (if detail
        (format "  %s %s: %s" mark label detail)
      (format "  %s %s" mark label))))

(defun exercism--self-check-key-help ()
  "Return key help for the self-check buffer."
  (let ((help "g rerun | c configure | q quit"))
    (when (exercism--setup-ok-p)
      (setq help (concat help " | t track")))
    (when (and (exercism--setup-ok-p) exercism--current-track)
      (setq help (concat help " | e exercises")))
    help))

(defun exercism--self-check-insert-heading ()
  "Insert the self-check report title and key help at point."
  (insert "Exercism Self-Check\n")
  (insert "===================\n\n")
  (insert (exercism--self-check-key-help))
  (insert "\n\n"))

(defun exercism--self-check-show-pending (status)
  "Show STATUS as the interim self-check report while async work runs."
  (with-current-buffer (exercism--self-check-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (exercism--self-check-insert-heading)
      (insert (propertize status 'face 'warning))
      (insert "\n")
      (goto-char (point-min)))))

(defun exercism--self-check-render ()
  "Redraw the self-check buffer from `exercism--self-check-results'."
  (let* ((results (reverse exercism--self-check-results))
         (failures (seq-filter (lambda (result) (not (nth 1 result))) results))
         (overall-ok (null failures)))
    (with-current-buffer (exercism--self-check-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (exercism--self-check-insert-heading)
        (insert (if overall-ok
                    (propertize "Overall: configured and reachable\n\n"
                                'face 'success)
                  (propertize (format "Overall: %d issue(s) found\n\n"
                                      (length failures))
                              'face 'error)))
        (dolist (result results)
          (insert (apply #'exercism--self-check-line result) "\n"))
        (goto-char (point-min))))))

(defun exercism--self-check-add (label ok-p &optional detail)
  "Record one self-check result and refresh the report buffer."
  (push (list label ok-p detail) exercism--self-check-results)
  (exercism--self-check-render))

(defun exercism--self-check-done ()
  "Mark one pending async self-check as finished."
  (setq exercism--self-check-pending (1- exercism--self-check-pending))
  (when (zerop exercism--self-check-pending)
    (message "[exercism] self-check complete — see *exercism-self-check*")))

(defun exercism--self-check-async (label thunk)
  "Run THUNK asynchronously and record its self-check result for LABEL."
  (setq exercism--self-check-pending (1+ exercism--self-check-pending))
  (make-thread
   (lambda ()
     (let ((result (funcall thunk)))
       (run-at-time 0 nil
                    (lambda ()
                      (exercism--self-check-add label (car result) (cdr result))
                      (exercism--self-check-done)))))))

(defun exercism-self-check-configure ()
  "Configure Exercism from the self-check buffer, then refresh the report."
  (interactive)
  (exercism--configure-interactive #'exercism-self-check))

(defun exercism-self-check-select-track ()
  "Open the track picker from the self-check buffer."
  (interactive)
  (unless (exercism--setup-ok-p)
    (user-error "Configure Exercism first (`c`)"))
  (exercism--prompt-for-track
   (lambda (track)
     (exercism--apply-track-selection
      track
      (lambda (selected-track)
        (exercism--set-current-track selected-track)
        (when (get-buffer-window (exercism--self-check-buffer) t)
          (exercism-self-check)))))))

(defun exercism-self-check-open-exercises ()
  "Open the exercise list from the self-check buffer."
  (interactive)
  (unless (exercism--setup-ok-p)
    (user-error "Configure Exercism first (`c`)"))
  (unless exercism--current-track
    (user-error "Select a track first (`t`)"))
  (exercism--open-exercise-list))

(defun exercism--self-check-reset ()
  "Clear accumulated self-check results and pending async count."
  (setq exercism--self-check-results nil
        exercism--self-check-pending 0))

(defun exercism--self-check-run-sync ()
  "Run local sync checks and record their self-check results."
  (let ((sync-results (exercism--sync-self-check-results)))
    (when sync-results
      (apply #'exercism--self-check-add (car sync-results))
      (apply #'exercism--self-check-add (exercism--cli-version-self-check-result))
      (dolist (result (cdr sync-results))
        (apply #'exercism--self-check-add result))))
  (when (exercism--setup-ok-p)
    (exercism--self-check-add "State file"
                              (file-exists-p exercism--state-file)
                              exercism--state-file)
    (when exercism--current-track
      (exercism--self-check-add "Current track" t exercism--current-track))
    (when exercism--current-exercise
      (exercism--self-check-add "Current exercise" t exercism--current-exercise))))

(defun exercism--self-check-probe-tracks ()
  "Probe the Exercism tracks API and record one self-check result."
  (setq exercism--self-check-pending (1+ exercism--self-check-pending))
  (request "https://exercism.org/api/v2/tracks"
    :parser #'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((tracks (exercism--plist-get data 'tracks)))
                  (exercism--self-check-add "Exercism API (tracks)"
                                            (and tracks (> (length tracks) 0))
                                            (format "%d tracks available"
                                                    (length tracks)))
                  (exercism--self-check-done))))
    :error (cl-function
            (lambda (&key error-thrown response &allow-other-keys)
              (exercism--self-check-add "Exercism API (tracks)" nil
                                        (exercism--request-error-message
                                         error-thrown response))
              (exercism--self-check-done)))))

(defun exercism--self-check-probe-ping ()
  "Probe the Exercism ping API and record one self-check result."
  (setq exercism--self-check-pending (1+ exercism--self-check-pending))
  (request "https://api.exercism.org/v1/ping"
    :success (cl-function
              (lambda (&key &allow-other-keys)
                (exercism--self-check-add "Exercism API (ping)" t "connected")
                (exercism--self-check-done)))
    :error (cl-function
            (lambda (&key error-thrown response &allow-other-keys)
              (exercism--self-check-add "Exercism API (ping)" nil
                                        (exercism--request-error-message
                                         error-thrown response))
              (exercism--self-check-done)))))

(defun exercism-self-check ()
  "Verify Exercism CLI setup and API connectivity, then show a report."
  (interactive)
  (exercism--self-check-reset)
  (pop-to-buffer (exercism--self-check-buffer))
  (exercism--self-check-render)
  (exercism--self-check-run-sync)
  (exercism--self-check-probe-tracks)
  (exercism--self-check-probe-ping))

(provide 'exercism-self-check)
;;; exercism-self-check.el ends here
