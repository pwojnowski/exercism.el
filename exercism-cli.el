;;; exercism-cli.el --- Exercism CLI boundary -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shell execution, configuration, download, version checks, and CLI
;; command construction for exercism.el.

;;; Code:

(require 'exercism-core)

(declare-function exercism--self-check-buffer "exercism-self-check")
(declare-function exercism--self-check-show-pending "exercism-self-check")

(defun exercism--cli-error-p (output)
  "Return non-nil if CLI OUTPUT is an error message."
  (string-match-p "^Error:" (string-trim output)))

(defun exercism--cli-already-exists-p (output)
  "Return non-nil if CLI OUTPUT says the target already exists."
  (string-match-p "already exists" output))

(defun exercism--run-shell-command (shell-cmd &optional callback)
  "Run SHELL-CMD asynchronously, calling CALLBACK with the output."
  (make-thread
   (lambda ()
     (let ((result (shell-command-to-string shell-cmd)))
       (when callback
         (run-at-time 0 nil callback result))))))

(defun exercism--cli-executable-path ()
  "Return the resolved Exercism CLI executable path, or nil."
  (or (executable-find exercism-executable)
      (when (file-executable-p exercism-executable)
        exercism-executable)))

(defun exercism--cli-version-from-output (output)
  "Return the version string parsed from CLI version OUTPUT, or nil."
  (when (string-match "exercism version \\([0-9.]+\\)" output)
    (match-string 1 output)))

(defun exercism--semver-to-number (semver)
  "Convert SEMVER (e.g. \"3.26.1\") to a comparable integer."
  (let ((portions (split-string semver "\\."))
        (portion-idx 0))
    (seq-reduce
     (lambda (sum portion)
       (prog1 (+ sum (* (expt 1000 portion-idx) (string-to-number portion)))
         (setq portion-idx (1+ portion-idx))))
     (reverse portions)
     0)))

(defun exercism--compare-semvers (ver1 op ver2)
  "Compare VER1 and VER2 with numeric operator OP."
  (funcall op (exercism--semver-to-number ver1)
           (exercism--semver-to-number ver2)))

(defun exercism--cli-version (callback)
  "Call CALLBACK with the installed Exercism CLI version string."
  (exercism--run-shell-command
   (concat (shell-quote-argument exercism-executable) " version")
   (lambda (result)
     (funcall callback (exercism--cli-version-from-output result)))))

(defun exercism--cli-version-self-check-result ()
  "Return a self-check result for the installed CLI version."
  (let ((label (format "CLI version (min %s)" exercism--min-cli-version))
        (exe (exercism--cli-executable-path)))
    (if (not exe)
        (list label nil "executable not found")
      (let ((output (shell-command-to-string
                      (concat (shell-quote-argument exercism-executable)
                              " version"))))
        (if-let ((version (exercism--cli-version-from-output output)))
            (if (exercism--compare-semvers version #'< exercism--min-cli-version)
                (list label nil
                      (format "%s (below min %s)" version exercism--min-cli-version))
              (list label t version))
          (list label nil (string-trim output)))))))

(defun exercism--configure (api-token workspace &optional after-callback)
  "Configure the Exercism CLI with API-TOKEN and WORKSPACE.
When AFTER-CALLBACK is provided, invoke it after configuration succeeds."
  (setq exercism--api-token api-token
        exercism--workspace (expand-file-name workspace))
  (message "[exercism] configuring... (please wait)")
  (when (get-buffer-window (exercism--self-check-buffer) t)
    (exercism--self-check-show-pending "Configuring... (please wait)"))
  (exercism--run-shell-command
   (concat (shell-quote-argument exercism-executable)
           " configure"
           " --token " (shell-quote-argument exercism--api-token)
           " --workspace " (shell-quote-argument exercism--workspace))
   (lambda (result)
     (message "[exercism] configure: %s" result)
     (exercism--sync-workspace-from-config)
     (when (file-exists-p exercism-config-path)
       (exercism--save-state))
     (when after-callback
       (funcall after-callback)))))

(defun exercism--configure-interactive (&optional after-callback)
  "Prompt for Exercism setup values and run `exercism--configure'.
When AFTER-CALLBACK is provided, invoke it after configuration succeeds."
  (let* ((api-token (read-string "API token: "))
         (default-workspace (exercism--workspace-configure-default))
         (workspace (expand-file-name
                     (read-directory-name "Workspace directory: "
                                          default-workspace
                                          default-workspace
                                          nil))))
    (exercism--configure api-token workspace after-callback)))

(defun exercism-configure ()
  "Configure the Exercism CLI."
  (interactive)
  (exercism--configure-interactive))

(defun exercism--download-exercise (exercise-slug track-slug callback)
  "Download EXERCISE-SLUG for TRACK-SLUG, then call CALLBACK with CLI output."
  (exercism--run-shell-command
   (concat (shell-quote-argument exercism-executable)
           " download"
           " --exercise=" (shell-quote-argument exercise-slug)
           " --track=" (shell-quote-argument track-slug))
   (lambda (result)
     (message "[exercism] download result for %s: %s" exercise-slug result)
     (funcall callback result))))

(defun exercism--build-submit-command (solution-files)
  "Return a shell command that submits SOLUTION-FILES with the Exercism CLI."
  (string-join
   (cons (concat (shell-quote-argument exercism-executable) " submit")
         (mapcar #'shell-quote-argument solution-files))
   " "))

(defun exercism--build-test-command ()
  "Return a shell command that runs Exercism CLI tests."
  (concat (shell-quote-argument exercism-executable) " test"))

(defun exercism--track-init (track-slug callback)
  "Initialize TRACK-SLUG by downloading hello-world, then call CALLBACK."
  (let ((hello-world-dir (expand-file-name "hello-world"
                                           (expand-file-name track-slug
                                                             exercism--workspace))))
    (if (file-directory-p hello-world-dir)
        (funcall callback "hello-world already present")
      (message "[exercism] initializing %s... (please wait)" track-slug)
      (exercism--download-exercise "hello-world" track-slug
                                     (lambda (result)
                                       (when (and (exercism--cli-error-p result)
                                                  (not (exercism--cli-already-exists-p result)))
                                         (user-error "%s" (string-trim result)))
                                       (funcall callback result))))))

(provide 'exercism-cli)
;;; exercism-cli.el ends here
