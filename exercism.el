;;; exercism.el --- Exercism.org CLI integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: Przemysław Wojnowski <esperanto@defun.tech>
;; Maintainer: Przemysław Wojnowski <esperanto@defun.tech>
;; Version: 1.0.0
;; Keywords: exercism, convenience
;; Homepage: https://github.com/pwojnowski/exercism.el
;; Package-Requires: ((emacs "29.1") (request "0.3.2"))

;;; Commentary:

;; Do Exercism exercises within Emacs via the `exercism' CLI.
;; Entry point: `M-x exercism' or `C-c x' (exercise list).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)

(defgroup exercism nil
  "Exercism.org CLI integration."
  :group 'tools)

(defcustom exercism-executable "exercism"
  "Executable name or path for the Exercism CLI."
  :type 'string
  :group 'exercism)

(defcustom exercism-config-path
  (expand-file-name "~/.config/exercism/user.json")
  "Path to the Exercism CLI user configuration file."
  :type 'file
  :group 'exercism)

(defvar exercism--state-file
  (expand-file-name "exercism-state.el" user-emacs-directory)
  "File persisting the current track, exercise, and workspace.")

(defvar exercism--api-token)
(defvar exercism--current-track nil)
(defvar exercism--current-exercise nil)
(defvar exercism--workspace
  (expand-file-name
   (if (eq system-type 'darwin) "~/Exercism" "~/exercism"))
  "Root directory for downloaded Exercism exercises.")

(defun exercism--save-state ()
  "Persist track, exercise, and workspace to `exercism--state-file'."
  (with-temp-file exercism--state-file
    (insert ";;; auto-generated — do not edit by hand\n\n")
    (insert (format "(setq exercism--current-track %S\n" exercism--current-track))
    (insert (format "      exercism--current-exercise %S\n" exercism--current-exercise))
    (insert (format "      exercism--workspace %S)\n" exercism--workspace))))

(defun exercism--load-state ()
  "Load persisted state from `exercism--state-file'."
  (when (file-exists-p exercism--state-file)
    (load exercism--state-file nil t)))

(defun exercism--read-user-config ()
  "Return parsed Exercism user config alist, or nil on error."
  (when (file-exists-p exercism-config-path)
    (condition-case nil
        (json-parse-string (exercism--file-to-string exercism-config-path)
                           :object-type 'alist
                           :array-type 'list)
      (error nil))))

(defun exercism--sync-workspace-from-config ()
  "Update `exercism--workspace' from the Exercism CLI user config."
  (let ((workspace (alist-get 'workspace (exercism--read-user-config))))
    (when workspace
      (setq exercism--workspace (expand-file-name workspace)))))

(defun exercism--workspace-tracks ()
  "Return track directory names under `exercism--workspace'."
  (when (file-directory-p exercism--workspace)
    (mapcar #'file-name-nondirectory
            (seq-filter #'file-directory-p
                        (directory-files exercism--workspace t "^[^.]")))))

(defun exercism--reconcile-state-with-config ()
  "Align persisted state with the Exercism CLI workspace configuration.
When the workspace saved in state differs from the CLI config, clear stale
track and exercise settings.  If the workspace contains a single track
directory, adopt it as the current track."
  (let ((state-workspace (expand-file-name exercism--workspace)))
    (exercism--sync-workspace-from-config)
    (let ((config-workspace (expand-file-name exercism--workspace)))
      (when (not (string-equal state-workspace config-workspace))
        (setq exercism--current-track nil
              exercism--current-exercise nil)
        (let ((tracks (exercism--workspace-tracks)))
          (when (= (length tracks) 1)
            (setq exercism--current-track (car tracks))))
        (exercism--save-state)))))

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

(defun exercism--file-to-string (file-path)
  "Return the contents of FILE-PATH as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun exercism--plist-get (plist key)
  "Return KEY from PLIST, an alist, or a mixed JSON object."
  (let ((sym (if (symbolp key) key (intern (format "%s" key)))))
    (or (plist-get plist sym)
        (plist-get plist key)
        (alist-get sym plist nil nil #'equal)
        (alist-get key plist nil nil #'equal))))

(defun exercism--configure (api-token)
  "Configure the Exercism CLI with API-TOKEN."
  (setq exercism--api-token api-token)
  (exercism--run-shell-command
   (concat (shell-quote-argument exercism-executable)
           " configure"
           " --token " (shell-quote-argument exercism--api-token))
   (lambda (result)
     (message "[exercism] configure: %s" result)
     (exercism--sync-workspace-from-config)
     (when (file-exists-p exercism-config-path)
       (exercism--save-state)))))

(defun exercism-configure ()
  "Configure the Exercism CLI."
  (interactive)
  (exercism--configure (read-string "API token: ")))

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

;; GET https://exercism.org/api/v2/tracks
;;
;; Sample response (one track, unauthenticated):
;;
;;   {"tracks":[{"slug":"emacs-lisp","title":"Emacs Lisp","course":false,
;;     "num_concepts":0,"num_exercises":86,
;;     "web_url":"https://exercism.org/tracks/emacs-lisp",
;;     "icon_url":"https://assets.exercism.org/tracks/emacs-lisp.svg",
;;     "tags":["Interpreted","Functional"],"last_touched_at":null,
;;     "is_new":false,
;;     "links":{"self":"https://exercism.org/tracks/emacs-lisp",
;;              "exercises":"https://exercism.org/tracks/emacs-lisp/exercises",
;;              "concepts":"https://exercism.org/tracks/emacs-lisp/concepts"}}]}
;;
;; Authenticated users who have joined a track also get per-track fields:
;; is_joined, num_learnt_concepts, num_completed_exercises,
;; has_notifications, and a non-null last_touched_at timestamp.

(defun exercism--list-tracks (callback)
  "Call CALLBACK with a list of track slug strings.

Uses GET https://exercism.org/api/v2/tracks.  The endpoint is public;
authentication is optional and this function does not send auth headers.

Response: a JSON object with a top-level `tracks' array.  Each track
object always includes `slug', `title', `course', `num_concepts',
`num_exercises', `web_url', `icon_url', `tags', `last_touched_at',
`is_new', and `links' (with `self', `exercises', and `concepts' URLs).

Without authentication, `last_touched_at' is null for every track.
When a Bearer token or session cookie identifies a user who has joined
a track, that track's object also includes `is_joined',
`num_learnt_concepts', `num_completed_exercises', and
`has_notifications', and `last_touched_at' is an ISO8601 timestamp.
Joined tracks are listed first.

Optional query params (not used here): `criteria', `tags', and `status'
(`status' requires authentication)."
  (request "https://exercism.org/api/v2/tracks"
    :parser #'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((tracks (exercism--plist-get data 'tracks))
                       (track-slugs (mapcar (lambda (track)
                                              (exercism--plist-get track 'slug))
                                            tracks)))
                  (funcall callback track-slugs))))))

;; GET https://exercism.org/api/v2/tracks/{slug}/exercises
;;
;; Sample response (one exercise, unauthenticated):
;;
;;   {"exercises":[{"slug":"hello-world","type":"tutorial","title":"Hello World",
;;     "icon_url":"https://assets.exercism.org/exercises/hello-world.svg",
;;     "difficulty":"easy",
;;     "blurb":"Exercism's classic introductory exercise...",
;;     "is_external":true,"is_unlocked":true,"is_recommended":false,
;;     "links":{"self":"/tracks/emacs-lisp/exercises/hello-world"}}]}
;;
;; Exercise `type' is `tutorial', `concept', or `practice'.  `difficulty' is
;; `easy', `medium', or `hard'.
;;
;; Without authentication, `is_external' is true, `is_unlocked' is true for
;; every exercise, and `is_recommended' is false.  With authentication on a
;; joined track, `is_unlocked' reflects progress, `is_recommended' marks at
;; most one next exercise, and `is_external' is false.
;;
;; Optional query params (not used here): `criteria' (search), `sideload'
;; (e.g. `sideload=solutions' adds a `solutions' array when authenticated).

(defun exercism--list-exercises (track-slug only-unlocked-p callback)
  "Call CALLBACK with exercise plists for TRACK-SLUG.

Uses GET https://exercism.org/api/v2/tracks/{slug}/exercises.  The endpoint
is public; authentication is optional and this function does not send auth
headers.

CALLBACK receives the `exercises' array from the response, optionally
filtered to unlocked exercises when ONLY-UNLOCKED-P is non-nil.  Each
exercise plist includes `slug', `type', `title', `icon_url', `difficulty',
`blurb', `is_external', `is_unlocked', `is_recommended', and `links'.

Without authentication, every exercise has `is_unlocked' set to true, so
ONLY-UNLOCKED-P has no effect unless auth headers are added later.
Authenticated users on a joined track get accurate `is_unlocked' values."
  (request (format "https://exercism.org/api/v2/tracks/%s/exercises" track-slug)
    :parser #'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((exercises (exercism--plist-get data 'exercises))
                       (filtered (seq-filter
                                  (lambda (exercise)
                                    (or (not only-unlocked-p)
                                        (exercism--plist-get exercise 'is_unlocked)))
                                  exercises)))
                  (funcall callback filtered))))))

(defun exercism--ensure-current-track ()
  "Signal an error unless `exercism--current-track' is set."
  (unless exercism--current-track
    (user-error "Set a track first (`t' in the exercise list, or `M-x exercism-exercise-list-set-track')")))

(defun exercism--get-api-token ()
  "Return the configured Exercism API token, or signal an error."
  (unless (and (boundp 'exercism--api-token) exercism--api-token)
    (let ((token (alist-get 'token (exercism--read-user-config))))
      (when (and token (not (string-empty-p token)))
        (setq exercism--api-token token))))
  (unless (and (boundp 'exercism--api-token) exercism--api-token)
    (user-error "Configure Exercism first (`M-x exercism-configure`)"))
  exercism--api-token)

(defun exercism--auth-headers ()
  "Return request headers for authenticated Exercism API calls."
  `(("Authorization" . ,(concat "Bearer " (exercism--get-api-token)))))

(defun exercism--list-solutions (track-slug callback)
  "Call CALLBACK with a slug->status hash table for TRACK-SLUG."
  (let ((solutions (make-hash-table :test 'equal)))
    (cl-labels
        ((fetch-page (page)
           (request
            (format "https://exercism.org/api/v2/solutions?track_slug=%s&page=%d"
                    (url-encode-url track-slug) page)
            :headers (exercism--auth-headers)
            :parser #'json-read
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        (let ((results (exercism--plist-get data 'results)))
                          (seq-doseq (solution results)
                            (let* ((exercise (exercism--plist-get solution 'exercise))
                                   (slug (exercism--plist-get exercise 'slug)))
                              (when slug
                                (puthash (exercism--json-value slug)
                                         (exercism--plist-get solution 'status)
                                         solutions))))
                        (let* ((meta (exercism--plist-get data 'meta))
                               (current-page (or (exercism--plist-get meta 'current_page) page))
                               (total-pages (or (exercism--plist-get meta 'total_pages) page)))
                          (if (< current-page total-pages)
                              (fetch-page (1+ current-page))
                            (funcall callback solutions))))))
            :error (cl-function
                    (lambda (&key error-thrown response &allow-other-keys)
                      (user-error "Failed to fetch solutions: %s"
                                  (or (when response
                                        (format "HTTP %s"
                                                (request-response-status-code response)))
                                      (format "%s" error-thrown))))))))
      (fetch-page 1))))

(defun exercism--exercise-list-solved-p (solution-status)
  "Return non-nil when SOLUTION-STATUS counts as completed on Exercism."
  (when solution-status
    (let ((status (downcase (exercism--json-value solution-status))))
      (member status '("published" "completed")))))

(defun exercism--exercise-list-state (exercise solution-status)
  "Return the display state symbol for EXERCISE and optional SOLUTION-STATUS."
  (cond
   ((exercism--exercise-list-solved-p solution-status) 'solved)
   ((not (exercism--plist-get exercise 'is_unlocked)) 'locked)
   ((null solution-status) 'not-started)
   (t 'in-progress)))

(defun exercism--exercise-list-state-label (state)
  "Return a propertized label for exercise list STATE."
  (pcase state
    ('solved (propertize "solved" 'face 'success))
    ('in-progress (propertize "in progress" 'face 'warning))
    ('not-started (propertize "not started" 'face 'shadow))
    ('locked (propertize "locked" 'face 'shadow))))

(defvar exercism--exercise-pending-states
  (make-hash-table :test 'equal)
  "Hash table mapping exercise slug to pending submit state.")

(defvar exercism--submit-animation-timer nil
  "Timer animating submitting rows in the exercise list.")

(defvar exercism--submit-animation-frame 0
  "Current animation frame for submitting status labels.")

(defun exercism--exercise-list-pending-label (state &optional frame)
  "Return a propertized label for pending submit STATE.
Optional FRAME cycles animation when STATE is `submitting'."
  (pcase state
    ('submitting
     (propertize (nth (% (or frame 0) 2) '("submitting " "submitting."))
                 'face 'warning))
    ('submitted (propertize "submitted" 'face 'success))
    ('submit-failed (propertize "failed" 'face 'error))))

(defun exercism--submitting-slugs ()
  "Return slugs currently in submitting state."
  (let (slugs)
    (maphash (lambda (slug state)
               (when (eq state 'submitting)
                 (push slug slugs)))
             exercism--exercise-pending-states)
    slugs))

(defun exercism--submit-animation-stop ()
  "Stop the submitting status animation timer."
  (when exercism--submit-animation-timer
    (cancel-timer exercism--submit-animation-timer)
    (setq exercism--submit-animation-timer nil)))

(defun exercism--submit-animation-update ()
  "Refresh submitting rows in the exercise list buffer."
  (setq exercism--submit-animation-frame (1+ exercism--submit-animation-frame))
  (dolist (slug (exercism--submitting-slugs))
    (exercism-exercise-list--set-pending-status slug 'submitting))
  (when (null (exercism--submitting-slugs))
    (exercism--submit-animation-stop)))

(defun exercism--submit-animation-start ()
  "Start the submitting status animation timer if needed."
  (unless exercism--submit-animation-timer
    (setq exercism--submit-animation-frame 0)
    (setq exercism--submit-animation-timer
          (run-with-timer 0.5 0.5 #'exercism--submit-animation-update))))

(defun exercism--submit-pending-set (slug state)
  "Record pending submit STATE for SLUG and update the exercise list row."
  (puthash slug state exercism--exercise-pending-states)
  (exercism-exercise-list--set-pending-status slug state)
  (when (eq state 'submitting)
    (exercism--submit-animation-start)))

(defvar exercism--exercise-list-buffer-name "*Exercism Exercises*"
  "Buffer name for exercise listings.")

(defvar exercism-exercise-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'exercism-exercise-list-open-exercise)
    (define-key map (kbd "n") #'exercism-exercise-list-next)
    (define-key map (kbd "p") #'exercism-exercise-list-previous)
    (define-key map (kbd "g") #'exercism-exercise-list-reload)
    (define-key map (kbd "d") #'exercism-download-all-unlocked-exercises)
    (define-key map (kbd "t") #'exercism-exercise-list-set-track)
    (define-key map (kbd "c") #'exercism-configure)
    (define-key map (kbd "s") #'exercism-exercise-list-submit-exercise)
    (define-key map (kbd "r") #'exercism-exercise-list-run-tests)
    (define-key map (kbd "S") #'exercism-exercise-list-submit-then-open-in-browser)
    (define-key map (kbd "b") #'exercism-exercise-list-open-in-browser)
    (define-key map (kbd "?") #'exercism-self-check)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `exercism-exercise-list-mode'.")

(defconst exercism-exercise-list-title "Exercism Exercises"
  "Title shown in the exercise list buffer.")

(defvar-local exercism-exercise-list-exercises nil
  "Cached exercise plists for the current exercise list buffer.")

(defvar-local exercism-exercise-list-solution-status-by-slug nil
  "Cached slug->status hash table for the current exercise list buffer.")

(defvar-local exercism-exercise-list-state-width 11
  "Width of the Status column in the exercise list buffer.")

(defun exercism-exercise-list--line-for-slug (slug)
  "Return (START . END) for the exercise row of SLUG, or nil."
  (when (and slug (get-buffer exercism--exercise-list-buffer-name))
    (with-current-buffer exercism--exercise-list-buffer-name
      (when (derived-mode-p 'exercism-exercise-list-mode)
        (save-excursion
          (goto-char (point-min))
          (catch 'found
            (while (not (eobp))
              (when (equal slug (get-text-property (point) 'exercism-exercise-slug))
                (throw 'found (cons (line-beginning-position)
                                    (line-end-position))))
              (forward-line 1))
            nil))))))

(defun exercism-exercise-list--set-pending-status (slug state)
  "Update the Status column for SLUG to pending submit STATE."
  (when-let ((bounds (exercism-exercise-list--line-for-slug slug)))
    (with-current-buffer exercism--exercise-list-buffer-name
      (let ((inhibit-read-only t)
            (width exercism-exercise-list-state-width)
            (unlocked-p (get-text-property (car bounds) 'exercism-exercise-unlocked))
            (label (if (eq state 'submitting)
                       (exercism--exercise-list-pending-label
                        state exercism--submit-animation-frame)
                     (exercism--exercise-list-pending-label state))))
        (delete-region (car bounds) (+ (car bounds) width))
        (goto-char (car bounds))
        (insert (format (format "%%-%ds" width) label))
        (add-text-properties (line-beginning-position) (line-end-position)
                               `(exercism-exercise-slug ,slug
                                 exercism-exercise-unlocked ,unlocked-p))))))

(defun exercism-exercise-list--exercise-line-p ()
  "Return non-nil when point is on an exercise row."
  (get-text-property (point) 'exercism-exercise-slug))

(defun exercism-exercise-list--slug-at-point ()
  "Return the exercise slug at point, or nil when not on an exercise row."
  (and (exercism-exercise-list--exercise-line-p)
       (get-text-property (point) 'exercism-exercise-slug)))

(defun exercism-exercise-list--goto-next-slug ()
  "Move point to the next exercise row, if any."
  (let ((next (next-single-property-change (point) 'exercism-exercise-slug)))
    (when next
      (goto-char next)
      (unless (exercism-exercise-list--exercise-line-p)
        (exercism-exercise-list--goto-next-slug)))))

(defun exercism-exercise-list--goto-previous-slug ()
  "Move point to the previous exercise row, if any."
  (let ((prev (previous-single-property-change (point) 'exercism-exercise-slug)))
    (when prev
      (goto-char prev)
      (unless (exercism-exercise-list--exercise-line-p)
        (exercism-exercise-list--goto-previous-slug)))))

(defun exercism-exercise-list-next ()
  "Move to the next exercise row."
  (interactive)
  (exercism-exercise-list--goto-next-slug))

(defun exercism-exercise-list-previous ()
  "Move to the previous exercise row."
  (interactive)
  (exercism-exercise-list--goto-previous-slug))

(defun exercism-exercise-list-open-exercise ()
  "Open the exercise on the current line, downloading if needed."
  (interactive)
  (let ((slug (exercism-exercise-list--slug-at-point))
        (unlocked-p (get-text-property (point) 'exercism-exercise-unlocked)))
    (unless slug
      (user-error "Not on an exercise row"))
    (unless unlocked-p
      (user-error "Exercise %s is locked" slug))
    (exercism--open-exercise-slug slug)))

(defun exercism-exercise-list-submit-exercise ()
  "Submit the exercise on the current line, after confirmation."
  (interactive)
  (let ((slug (exercism-exercise-list--slug-at-point))
        (unlocked-p (get-text-property (point) 'exercism-exercise-unlocked)))
    (unless slug
      (user-error "Not on an exercise row"))
    (unless unlocked-p
      (user-error "Exercise %s is locked" slug))
    (when (y-or-n-p (format "Submit exercise %s on track %s? "
                            slug exercism--current-track))
      (exercism--submit-slug slug))))

(defun exercism-exercise-list-submit-then-open-in-browser ()
  "Submit the exercise on the current line, then open it in a browser."
  (interactive)
  (let ((slug (exercism-exercise-list--slug-at-point))
        (unlocked-p (get-text-property (point) 'exercism-exercise-unlocked)))
    (unless slug
      (user-error "Not on an exercise row"))
    (unless unlocked-p
      (user-error "Exercise %s is locked" slug))
    (when (y-or-n-p (format "Submit exercise %s on track %s and open in browser? "
                            slug exercism--current-track))
      (exercism--submit-slug slug t))))

(defun exercism--exercise-url (track-slug exercise-slug)
  "Return the Exercism.org URL for EXERCISE-SLUG on TRACK-SLUG."
  (format "https://exercism.org/tracks/%s/exercises/%s"
          track-slug exercise-slug))

(defun exercism-exercise-list-open-in-browser ()
  "Open the exercise on the current line in a browser."
  (interactive)
  (exercism--ensure-current-track)
  (let ((slug (exercism-exercise-list--slug-at-point)))
    (unless slug
      (user-error "Not on an exercise row"))
    (browse-url (exercism--exercise-url exercism--current-track slug))))

(defun exercism-exercise-list-reload ()
  "Reload the exercise list in the current buffer."
  (interactive)
  (unless (derived-mode-p 'exercism-exercise-list-mode)
    (user-error "Not in Exercism exercise list buffer"))
  (exercism--submit-animation-stop)
  (clrhash exercism--exercise-pending-states)
  (exercism--with-track-exercises-and-solutions
   (lambda (exercises solution-status-by-slug)
     (exercism--show-exercise-list exercises solution-status-by-slug))))

(defun exercism--exercise-list-apply-track (track)
  "Set TRACK as current, persist state, and reload the exercise list."
  (setq exercism--current-track track)
  (exercism--save-state)
  (message "[exercism] set current track to: %s" track)
  (exercism-exercise-list-reload))

(defun exercism-exercise-list-set-track ()
  "Set the current Exercism track and reload the exercise list."
  (interactive)
  (unless (derived-mode-p 'exercism-exercise-list-mode)
    (user-error "Not in Exercism exercise list buffer"))
  (exercism--list-tracks
   (lambda (tracks)
     (exercism--sync-workspace-from-config)
     (let* ((track (completing-read "Choose track: " tracks nil t))
            (track-dir (expand-file-name track exercism--workspace)))
       (if (file-exists-p track-dir)
           (exercism--exercise-list-apply-track track)
         (exercism--track-init
          track
          (lambda (_result)
            (exercism--exercise-list-apply-track track))))))))

(define-derived-mode exercism-exercise-list-mode special-mode "Exercism Exercises"
  "Major mode for browsing Exercism exercises."
  (setq buffer-read-only t)
  (hl-line-mode 1))

(defun exercism--exercise-list-longest (exercises property)
  "Return the longest PROPERTY string length among EXERCISES."
  (apply #'max 0
         (mapcar (lambda (exercise)
                   (length (exercism--json-value
                            (exercism--plist-get exercise property))))
                 exercises)))

(defun exercism--exercise-list-counts (exercises solution-status-by-slug)
  "Return (SOLVED-COUNT UNSOLVED-COUNT) for all EXERCISES."
  (let ((solved-count
         (seq-count
          (lambda (exercise)
            (let ((slug (exercism--json-value
                         (exercism--plist-get exercise 'slug))))
              (exercism--exercise-list-solved-p
               (gethash slug solution-status-by-slug))))
          exercises)))
    (list solved-count (- (length exercises) solved-count))))

(defun exercism--order-exercises (exercises solution-status-by-slug)
  "Return EXERCISES with solved entries last, preserving relative order."
  (append
   (seq-filter
    (lambda (exercise)
      (let ((slug (exercism--json-value
                   (exercism--plist-get exercise 'slug))))
        (not (exercism--exercise-list-solved-p
              (gethash slug solution-status-by-slug)))))
    exercises)
   (seq-filter
    (lambda (exercise)
      (let ((slug (exercism--json-value
                   (exercism--plist-get exercise 'slug))))
        (exercism--exercise-list-solved-p
         (gethash slug solution-status-by-slug))))
    exercises)))

(defun exercism--render-exercise-list ()
  "Redraw the exercise list buffer from cached data."
  (let* ((exercises exercism-exercise-list-exercises)
         (solution-status-by-slug exercism-exercise-list-solution-status-by-slug)
         (ordered (exercism--order-exercises
                   exercises solution-status-by-slug))
         (state-width 11)
         (difficulty-width (exercism--exercise-list-longest ordered 'difficulty))
         (slug-width (exercism--exercise-list-longest ordered 'slug))
         (counts (exercism--exercise-list-counts exercises solution-status-by-slug))
         (solved-count (car counts))
         (unsolved-count (cadr counts))
         (title exercism-exercise-list-title))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert title "\n")
      (insert (make-string (length title) ?=) "\n\n")
      (insert "RET open | b browser | s submit | S submit+browser | r test | d download all | n/p move | g reload | t track | c configure | ? self-check | q quit\n\n")
      (insert (format "Track: %s\n" exercism--current-track))
      (insert (format "Exercises: %d | Solved: %d | Unsolved: %d\n\n"
                      (length exercises)
                      solved-count unsolved-count))
      (insert (format (format "%%-%ds  %%-%ds  %%-%ds  %%s\n"
                              state-width difficulty-width slug-width)
                      "Status" "Difficulty" "Exercise" "Blurb"))
      (insert (make-string (+ state-width difficulty-width slug-width 8) ?-) "\n")
      (seq-doseq (exercise ordered)
        (let* ((slug (exercism--json-value (exercism--plist-get exercise 'slug)))
               (difficulty (exercism--json-value (exercism--plist-get exercise 'difficulty)))
               (blurb (exercism--plist-get exercise 'blurb))
               (solution-status (gethash slug solution-status-by-slug))
               (state (exercism--exercise-list-state exercise solution-status))
               (unlocked-p (exercism--plist-get exercise 'is_unlocked))
               (line-start (point))
               (difficulty-face (pcase difficulty
                                  ("easy" '(:foreground "green"))
                                  ("medium" '(:foreground "yellow"))
                                  ("hard" '(:foreground "red"))
                                  (_ '(:foreground "blue")))))
          (insert (format (format "%%-%ds  " state-width)
                          (exercism--exercise-list-state-label state))
                  (propertize (format (format "%%-%ds" difficulty-width) difficulty)
                              'face difficulty-face)
                  "  "
                  (format (format "%%-%ds" slug-width) slug)
                  "  "
                  (propertize blurb 'face 'shadow)
                  "\n")
          (add-text-properties line-start (point)
                               `(exercism-exercise-slug ,slug
                                 exercism-exercise-unlocked ,unlocked-p))))
      (setq exercism-exercise-list-state-width state-width)
      (goto-char (point-min))
      (catch 'found
        (while (not (eobp))
          (when (get-text-property (point) 'exercism-exercise-slug)
            (throw 'found t))
          (forward-line 1))))))

(defun exercism--show-exercise-list (exercises solution-status-by-slug)
  "Cache EXERCISES and SOLUTION-STATUS-BY-SLUG, then render the list."
  (with-current-buffer (get-buffer-create exercism--exercise-list-buffer-name)
    (exercism-exercise-list-mode)
    (setq exercism-exercise-list-exercises exercises
          exercism-exercise-list-solution-status-by-slug solution-status-by-slug)
    (exercism--render-exercise-list)
    (pop-to-buffer (current-buffer))))

(defun exercism--with-track-exercises-and-solutions (callback)
  "Fetch all exercises and solution statuses, then call CALLBACK."
  (exercism--ensure-current-track)
  (exercism--get-api-token)
  (message "[exercism] loading exercises for %s..." exercism--current-track)
  (exercism--list-exercises exercism--current-track nil
   (lambda (exercises)
     (exercism--list-solutions exercism--current-track
      (lambda (solution-status-by-slug)
        (funcall callback exercises solution-status-by-slug))))))

(defun exercism ()
  "Open the Exercism exercise list for the current track."
  (interactive)
  (exercism--with-track-exercises-and-solutions
   (lambda (exercises solution-status-by-slug)
     (exercism--show-exercise-list exercises solution-status-by-slug))))

(defun exercism--get-config (exercise-dir)
  "Return the parsed config alist for EXERCISE-DIR."
  (json-parse-string
   (exercism--file-to-string
    (expand-file-name ".exercism/config.json" exercise-dir))
   :object-type 'alist
   :array-type 'list))

(defun exercism--get-solution-files (exercise-dir)
  "Return solution file paths for EXERCISE-DIR."
  (let ((config (exercism--get-config exercise-dir)))
    (alist-get 'solution (alist-get 'files config))))

(defun exercism--solution-file-paths (exercise-dir)
  "Return absolute solution file paths for EXERCISE-DIR."
  (let ((solution-files (exercism--get-solution-files exercise-dir)))
    (when solution-files
      (let ((files (if (listp solution-files) solution-files (list solution-files))))
        (mapcar (lambda (file) (expand-file-name file exercise-dir)) files)))))

(defun exercism--primary-solution-file (exercise-dir)
  "Return the primary solution file path for EXERCISE-DIR, or nil."
  (car (exercism--solution-file-paths exercise-dir)))

(defun exercism--open-exercise-dir (exercise-dir)
  "Visit EXERCISE-DIR by opening its primary solution file."
  (let ((solution-file (exercism--primary-solution-file exercise-dir)))
    (if solution-file
        (find-file solution-file)
      (user-error "No solution file found in %s" exercise-dir))))

(defun exercism--submit-complete (slug result open-in-browser-after-p)
  "Finalize submit for SLUG with CLI RESULT."
  (let ((final-state (if (exercism--cli-error-p result) 'submit-failed 'submitted)))
    (exercism--submit-pending-set slug final-state)
    (when (null (exercism--submitting-slugs))
      (exercism--submit-animation-stop))
    (if (exercism--cli-error-p result)
        (message "[exercism] submit failed: %s" (string-trim result))
      (message "[exercism] submit succeeded: %s" (string-trim result)))
    (setq exercism--current-exercise slug)
    (exercism--save-state)
    (when (and open-in-browser-after-p
               (string-match "\\(https://exercism\\.org.*\\)" result))
      (browse-url (match-string 1 result)))))

(defun exercism--submit-slug (slug &optional open-in-browser-after-p)
  "Submit SLUG on `exercism--current-track'."
  (exercism--ensure-current-track)
  (when (eq (gethash slug exercism--exercise-pending-states) 'submitting)
    (message "[exercism] already submitting %s" slug)
    (cl-return-from exercism--submit-slug))
  (let* ((track-dir (expand-file-name exercism--current-track exercism--workspace))
         (exercise-dir (expand-file-name slug track-dir)))
    (unless (file-directory-p exercise-dir)
      (user-error "Exercise %s is not downloaded" slug))
    (let* ((solution-files (exercism--solution-file-paths exercise-dir))
           (default-directory exercise-dir)
           (submit-command (string-join
                            (cons (concat (shell-quote-argument exercism-executable)
                                          " submit")
                                  (mapcar #'shell-quote-argument solution-files))
                            " ")))
      (unless solution-files
        (user-error "No solution file found in %s" exercise-dir))
      (exercism--submit-pending-set slug 'submitting)
      (message "[exercism] submitting %s on %s..."
               slug exercism--current-track)
      (exercism--run-shell-command
       submit-command
       (lambda (result)
         (exercism--submit-complete slug result open-in-browser-after-p))))))

(defun exercism--submit (&optional open-in-browser-after-p)
  "Submit the solution in the current exercise directory."
  (unless exercism--current-exercise
    (user-error "No current exercise"))
  (exercism--submit-slug exercism--current-exercise open-in-browser-after-p))

(defun exercism-submit ()
  "Submit your implementation."
  (interactive)
  (exercism--submit))

(defun exercism-submit-then-open-in-browser ()
  "Submit your implementation, then open the submission page in a browser."
  (interactive)
  (exercism--submit t))

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

(defun exercism--json-value (value)
  "Return a string representation of JSON VALUE."
  (cond ((stringp value) value)
        ((symbolp value) (symbol-name value))
        ((numberp value) (number-to-string value))
        (t (format "%s" value))))

(defun exercism--open-exercise-slug (slug)
  "Download SLUG on the current track if needed, then open it."
  (let* ((track-dir (expand-file-name exercism--current-track exercism--workspace))
         (exercise-dir (expand-file-name slug track-dir)))
    (if (file-exists-p exercise-dir)
        (progn
          (exercism--open-exercise-dir exercise-dir)
          (setq exercism--current-exercise slug)
          (exercism--save-state))
      (message "[exercism] downloading %s exercise %s... (please wait)"
               exercism--current-track slug)
      (exercism--download-exercise
       slug exercism--current-track
       (lambda (result)
         (message "[exercism] download result: %s" result)
         (when (file-exists-p exercise-dir)
           (exercism--open-exercise-dir exercise-dir))
         (setq exercism--current-exercise slug)
         (exercism--save-state))))))

(defun exercism-download-all-unlocked-exercises ()
  "Download all unlocked exercises for the current track."
  (interactive)
  (unless exercism--current-track
    (user-error "Set a track first (`t' in the exercise list, or `M-x exercism-exercise-list-set-track')"))
  (exercism--list-exercises
   exercism--current-track t
   (lambda (track-exercises)
     (let ((track-dir (expand-file-name exercism--current-track exercism--workspace)))
       (seq-doseq (exercise track-exercises)
         (let* ((slug (exercism--json-value (exercism--plist-get exercise 'slug)))
                (exercise-dir (expand-file-name slug track-dir)))
           (unless (file-exists-p exercise-dir)
             (message "[exercism] attempting to download %s exercise %s..."
                      exercism--current-track slug)
             (exercism--download-exercise slug exercism--current-track
                                          (lambda (_result) nil)))))))))

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
     (funcall callback
              (when (string-match "exercism version \\([0-9.]+\\)" result)
                (match-string 1 result))))))

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

(defun exercism--self-check-buffer ()
  "Return the `*exercism-self-check*' buffer, creating it if needed."
  (or (get-buffer "*exercism-self-check*")
      (let ((buf (generate-new-buffer "*exercism-self-check*")))
        (with-current-buffer buf
          (special-mode))
        buf)))

(defun exercism--self-check-line (label ok-p &optional detail)
  "Format one self-check result line for LABEL, OK-P, and optional DETAIL."
  (let ((mark (if ok-p
                  (propertize "✓" 'face 'success)
                (propertize "✗" 'face 'error))))
    (if detail
        (format "  %s %s: %s" mark label detail)
      (format "  %s %s" mark label))))

(defun exercism--self-check-render ()
  "Redraw the self-check buffer from `exercism--self-check-results'."
  (let* ((results (reverse exercism--self-check-results))
         (failures (seq-filter (lambda (result) (not (nth 1 result))) results))
         (overall-ok (null failures)))
    (with-current-buffer (exercism--self-check-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Exercism Self-Check\n")
        (insert "===================\n\n")
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

(defun exercism-self-check ()
  "Verify Exercism CLI setup and API connectivity, then show a report."
  (interactive)
  (setq exercism--self-check-results nil
        exercism--self-check-pending 0)
  (pop-to-buffer (exercism--self-check-buffer))
  (exercism--self-check-render)
  (let* ((exe (or (executable-find exercism-executable)
                  (when (file-executable-p exercism-executable)
                    exercism-executable)))
         (user-config (exercism--read-user-config))
         (token (when user-config (alist-get 'token user-config)))
         (workspace (or (when user-config (alist-get 'workspace user-config))
                        exercism--workspace)))
    (exercism--self-check-add "CLI executable" (not (null exe)) (or exe exercism-executable))
    (exercism--self-check-add "Config file"
                              (file-exists-p exercism-config-path)
                              exercism-config-path)
    (exercism--self-check-add "API token configured"
                              (and token (not (string-empty-p token)))
                              (if token
                                  (exercism--masked-token token)
                                "missing from config"))
    (when workspace
      (exercism--self-check-add "Workspace directory"
                                (file-directory-p workspace)
                                workspace))
    (when exercism--current-track
      (exercism--self-check-add "Current track" t exercism--current-track))
    (when exercism--current-exercise
      (exercism--self-check-add "Current exercise" t exercism--current-exercise)))
  (exercism--self-check-async
   "CLI version"
   (lambda ()
     (let ((output (shell-command-to-string
                    (concat (shell-quote-argument exercism-executable) " version"))))
       (if (string-match "exercism version \\([0-9.]+\\)" output)
           (cons t (match-string 1 output))
         (cons nil (string-trim output))))))
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
                                        (or (when response
                                              (format "HTTP %s"
                                                      (request-response-status-code
                                                       response)))
                                            (format "%s" error-thrown)))
              (exercism--self-check-done))))
  (setq exercism--self-check-pending (1+ exercism--self-check-pending))
  (request "https://api.exercism.org/v1/ping"
    :success (cl-function
              (lambda (&key &allow-other-keys)
                (exercism--self-check-add "Exercism API (ping)" t "connected")
                (exercism--self-check-done)))
    :error (cl-function
            (lambda (&key error-thrown response &allow-other-keys)
              (exercism--self-check-add "Exercism API (ping)" nil
                                        (or (when response
                                              (format "HTTP %s"
                                                      (request-response-status-code
                                                       response)))
                                            (format "%s" error-thrown)))
              (exercism--self-check-done)))))

(defun exercism--run-tests-in-dir (exercise-dir)
  "Run Exercism CLI tests in EXERCISE-DIR."
  (exercism--cli-version
   (lambda (version)
     (let ((min-version "3.2.0"))
       (cond
        ((not version)
         (message "[exercism] error: could not determine CLI version"))
        ((exercism--compare-semvers version #'< min-version)
         (message "[exercism] error: running tests requires CLI %s+ (you have %s)"
                  min-version version))
        (t
         (let* ((default-directory exercise-dir)
                (compile-command (concat (shell-quote-argument exercism-executable)
                                         " test")))
           (compile compile-command))))))))

(defun exercism--run-tests-for-slug (slug)
  "Run tests for SLUG on `exercism--current-track'."
  (exercism--ensure-current-track)
  (let ((exercise-dir (expand-file-name slug
                                        (expand-file-name exercism--current-track
                                                          exercism--workspace))))
    (unless (file-directory-p exercise-dir)
      (user-error "Exercise %s is not downloaded" slug))
    (exercism--run-tests-in-dir exercise-dir)))

(defun exercism-run-tests ()
  "Run tests for the current exercise."
  (interactive)
  (unless exercism--current-exercise
    (user-error "No current exercise"))
  (exercism--run-tests-for-slug exercism--current-exercise))

(defun exercism-exercise-list-run-tests ()
  "Run tests for the exercise on the current line."
  (interactive)
  (let ((slug (exercism-exercise-list--slug-at-point))
        (unlocked-p (get-text-property (point) 'exercism-exercise-unlocked)))
    (unless slug
      (user-error "Not on an exercise row"))
    (unless unlocked-p
      (user-error "Exercise %s is locked" slug))
    (exercism--run-tests-for-slug slug)))

(exercism--load-state)
(exercism--reconcile-state-with-config)

(provide 'exercism)
;;; exercism.el ends here
