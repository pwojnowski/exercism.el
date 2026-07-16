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
(require 'url)
(require 'xdg nil t)

(defconst exercism--http-user-agent "Mozilla/5.0 (compatible; exercism.el/1.0)"
  "User-Agent sent when downloading static assets from Exercism.")

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

(defconst exercism--min-cli-version "3.2.0"
  "Minimum Exercism CLI version required by exercism.el.")

(defvar exercism--state-file
  (expand-file-name "exercism-state.el" user-emacs-directory)
  "File persisting the current track, exercise, and workspace.")

(defconst exercism--default-workspace
  (expand-file-name "~/Exercism")
  "Default Exercism workspace directory (matches the CLI default).")

(defvar exercism--api-token)
(defvar exercism--current-track nil)
(defvar exercism--current-exercise nil)
(defvar exercism--workspace exercism--default-workspace
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

(defun exercism--workspace-from-state ()
  "Return workspace from `exercism--state-file' without changing live state."
  (when (file-exists-p exercism--state-file)
    (let ((track exercism--current-track)
          (exercise exercism--current-exercise)
          (workspace exercism--workspace)
          (loaded nil))
      (load exercism--state-file nil t)
      (setq loaded exercism--workspace
            exercism--current-track track
            exercism--current-exercise exercise
            exercism--workspace workspace)
      (when loaded
        (expand-file-name loaded)))))

(defun exercism--workspace-configure-default ()
  "Return the default workspace directory for `exercism-configure'."
  (or (exercism--workspace-from-state) exercism--default-workspace))

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

(defun exercism--list-tracks (callback &optional error-callback)
  "Call CALLBACK with a list of track plists from GET /api/v2/tracks.

Uses GET https://exercism.org/api/v2/tracks.  The endpoint is public;
when an API token is configured, auth headers are sent so joined-track
fields are included.

CALLBACK receives the `tracks' array from the response.  Each track
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
(`status' requires authentication).

When ERROR-CALLBACK is provided, it is called with the same keyword
arguments as `request' error handlers instead of signaling."
  (let ((url "https://exercism.org/api/v2/tracks")
        (headers (exercism--maybe-auth-headers))
        (success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (funcall callback (exercism--plist-get data 'tracks)))))
        (error-handler
         (cl-function
          (lambda (&key error-thrown response &allow-other-keys)
            (if error-callback
                (funcall error-callback
                         :error-thrown error-thrown
                         :response response)
              (user-error "Failed to fetch tracks: %s"
                          (or (when response
                                (format "HTTP %s"
                                        (request-response-status-code response)))
                              (format "%s" error-thrown))))))))
    (if headers
        (request url :headers headers :parser #'json-read
                 :success success :error error-handler)
      (request url :parser #'json-read :success success :error error-handler))))

(defvar exercism--track-icon-size 16
  "Height and width in pixels for track icons in the track list.")

(defvar exercism--track-icon-cache-root nil
  "When non-nil, override the root directory for cached track icons.")

(defun exercism--user-cache-dir ()
  "Return the user cache directory, preferring XDG when available."
  (or exercism--track-icon-cache-root
      (if (fboundp 'xdg-user-dirs-cache-dir)
          (xdg-user-dirs-cache-dir)
        (expand-file-name ".cache/" "~"))))

(defun exercism--track-icon-cache-dir ()
  "Return and ensure the XDG cache directory for track icons."
  (let ((dir (expand-file-name "exercism/track-icons/"
                               (exercism--user-cache-dir))))
    (make-directory dir t)
    dir))

(defun exercism--track-icon-cache-path (slug)
  "Return the cache file path for track SLUG icon."
  (expand-file-name (format "%s.svg" slug) (exercism--track-icon-cache-dir)))

(defun exercism--svg-file-p (path)
  "Return non-nil when PATH looks like an SVG file."
  (and (file-exists-p path)
       (with-temp-buffer
         (condition-case nil
             (progn
               (insert-file-contents path nil 0 512)
               (goto-char (point-min))
               (looking-at "\\`\\s-*<svg"))
           (error nil)))))

(defun exercism--asset-request-headers ()
  "Return HTTP headers for Exercism static asset downloads."
  `(("User-Agent" . ,exercism--http-user-agent)))

(defun exercism--track-icon-image (path)
  "Return an image for cached SVG PATH, or nil when unavailable."
  (when (and (image-type-available-p 'svg)
             (exercism--svg-file-p path))
    (condition-case nil
        (create-image (exercism--file-to-string path) 'svg t
                      :ascent 'center
                      :height exercism--track-icon-size
                      :width exercism--track-icon-size)
      (error nil))))

(defun exercism--track-icon-fallback-display (slug)
  "Return a text fallback when track SLUG icon cannot be rendered."
  (propertize (format "%2s" (upcase (substring slug 0 1)))
              'face 'font-lock-constant-face))

(defun exercism--fetch-track-icon (slug icon-url callback)
  "Ensure SLUG icon is cached from ICON-URL, then call CALLBACK with path."
  (let ((path (exercism--track-icon-cache-path slug)))
    (if (exercism--svg-file-p path)
        (funcall callback path)
      (when (file-exists-p path)
        (delete-file path))
      (let ((url-request-extra-headers (exercism--asset-request-headers)))
        (url-retrieve
         icon-url
         (lambda (status)
           (unwind-protect
               (unless (plist-get status :error)
                 (goto-char (point-min))
                 (when (re-search-forward "\r?\n\r?\n" nil t)
                   (let ((data (buffer-substring-no-properties
                                (point) (point-max))))
                     (when (string-match-p "\\`\\s-*<svg" data)
                       (with-temp-file path
                         (insert data))))))
             (when (buffer-live-p (current-buffer))
               (kill-buffer (current-buffer))))
           (funcall callback (and (exercism--svg-file-p path) path)))
         nil t t)))))

(defun exercism--track-icon-display (slug)
  "Return a propertized inline display for cached track SLUG icon."
  (let ((path (exercism--track-icon-cache-path slug)))
    (if-let ((image (exercism--track-icon-image path)))
        (propertize " " 'display (list image))
      (exercism--track-icon-fallback-display slug))))

(defun exercism--track-icon-separator ()
  "Return a space aligning track titles after the fixed-width icon column."
  (propertize
   " " 'display
   `(space :align-to
           ,(list (+ exercism--track-icon-size (frame-char-width))))))

(defun exercism--prefetch-track-icons (tracks)
  "Download missing icons for TRACKS and refresh the track list buffer."
  (let* ((tracks-with-icons
          (seq-filter
           (lambda (track)
             (and (exercism--plist-get track 'slug)
                  (exercism--plist-get track 'icon_url)))
           tracks))
         (remaining (length tracks-with-icons)))
    (seq-doseq (track tracks-with-icons)
      (let ((slug (exercism--json-value (exercism--plist-get track 'slug)))
            (icon-url (exercism--plist-get track 'icon_url)))
        (exercism--fetch-track-icon
         slug icon-url
         (lambda (_path)
           (setq remaining (1- remaining))
           (when (zerop remaining)
             (when-let ((buffer (get-buffer exercism--track-list-buffer-name)))
               (with-current-buffer buffer
                 (when (derived-mode-p 'exercism-track-list-mode)
                   (exercism--render-track-list)))))))))))

(defun exercism--track-list-enrollment-label (is-joined auth-present-p)
  "Return enrollment label for IS-JOINED when AUTH-PRESENT-P."
  (cond ((not auth-present-p) "—")
        ((exercism--json-bool is-joined) (propertize "Joined" 'face 'success))
        (t (propertize "Not joined" 'face 'shadow))))

(defun exercism--track-list-progress-count (value)
  "Return progress count VALUE as a string, treating nil as zero."
  (number-to-string (if (numberp value) value 0)))

(defun exercism--track-list-show-progress-p (auth-present-p is-joined)
  "Return non-nil when AUTH-PRESENT-P and IS-JOINED warrant progress display."
  (and auth-present-p (exercism--json-bool is-joined)))

(defun exercism--track-list-progress-label (learnt total show-progress-p)
  "Return progress label for LEARNT/TOTAL when SHOW-PROGRESS-P."
  (if show-progress-p
      (format "%s/%s"
              (exercism--track-list-progress-count learnt)
              (exercism--track-list-progress-count total))
    (exercism--track-list-progress-count total)))

(defun exercism--track-list-concepts-label (learnt total show-progress-p)
  "Return concepts progress, or an em dash when TOTAL is zero."
  (if (and (numberp total) (zerop total))
      "—"
    (exercism--track-list-progress-label learnt total show-progress-p)))

(defun exercism--track-list-type-label (course-p)
  "Return track type label for COURSE-P."
  (if (exercism--json-bool course-p) "course" "practice"))

(defun exercism--track-list-is-new-label (is-new-p)
  "Return propertized new-track badge label for IS-NEW-P."
  (if (exercism--json-bool is-new-p)
      (propertize "new" 'face 'warning)
    ""))

(defun exercism--track-list-notifications-label (has-notifications auth-present-p)
  "Return propertized notifications label for HAS-NOTIFICATIONS."
  (cond ((not auth-present-p) "—")
        ((exercism--json-bool has-notifications) (propertize "notify" 'face 'error))
        (t "")))

(defun exercism--track-list-last-touched-label (timestamp)
  "Return a short date label for TIMESTAMP, or \"—\" when absent."
  (let ((value (and timestamp (not (eq timestamp :null))
                    (exercism--json-value timestamp))))
    (if (and value (string-match "\\`\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" value))
        (match-string 1 value)
      "—")))

(defun exercism--track-list-label-width (label)
  "Return display width of propertized track list LABEL."
  (length (substring-no-properties (or label ""))))

(defun exercism--track-list-pad-label (label width)
  "Return LABEL padded with spaces to WIDTH display columns."
  (let* ((text (substring-no-properties (or label "")))
         (padding (make-string (max 0 (- width (length text))) ?\s)))
    (if (string-empty-p text)
        (make-string width ?\s)
      (concat label padding))))

(defun exercism--track-list-pad-right (label width)
  "Return plain LABEL right-aligned within WIDTH display columns."
  (format (format "%%%ds" width)
          (substring-no-properties (or label ""))))

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

(defun exercism--load-api-token ()
  "Return the configured Exercism API token, or nil when unset."
  (unless (and (boundp 'exercism--api-token) exercism--api-token)
    (let ((token (alist-get 'token (exercism--read-user-config))))
      (when (and token (not (string-empty-p token)))
        (setq exercism--api-token token))))
  (and (boundp 'exercism--api-token) exercism--api-token))

(defun exercism--get-api-token ()
  "Return the configured Exercism API token, or signal an error."
  (or (exercism--load-api-token)
      (user-error "Configure Exercism first (`M-x exercism-configure`)")))

(defun exercism--maybe-get-api-token ()
  "Return the configured Exercism API token, or nil when unset."
  (exercism--load-api-token))

(defun exercism--auth-headers ()
  "Return request headers for authenticated Exercism API calls."
  `(("Authorization" . ,(concat "Bearer " (exercism--get-api-token)))))

(defun exercism--maybe-auth-headers ()
  "Return auth headers when a token is configured, otherwise nil."
  (when-let ((token (exercism--maybe-get-api-token)))
    `(("Authorization" . ,(concat "Bearer " token)))))

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
     (exercism--show-exercise-list exercises solution-status-by-slug 'no-display))))

(defun exercism--exercise-list-apply-track (track)
  "Set TRACK as current, persist state, and reload the exercise list."
  (setq exercism--current-track track)
  (exercism--save-state)
  (message "[exercism] set current track to: %s" track)
  (exercism-exercise-list-reload))

(defun exercism--exercise-list-apply-track-in-buffer (track buffer)
  "Apply TRACK in BUFFER when it is a live exercise list buffer."
  (unless (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (derived-mode-p 'exercism-exercise-list-mode)))
    (user-error "Buffer is not a live Exercism exercise list buffer"))
  (with-current-buffer buffer
    (exercism--exercise-list-apply-track track)))

(defvar exercism--track-list-buffer-name "*Exercism Tracks*"
  "Buffer name for track listings.")

(defvar exercism-track-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'exercism-track-list-select-track)
    (define-key map (kbd "n") #'exercism-track-list-next)
    (define-key map (kbd "p") #'exercism-track-list-previous)
    (define-key map (kbd "g") #'exercism-track-list-reload)
    (define-key map (kbd "q") #'exercism-track-list-cancel)
    map)
  "Keymap for `exercism-track-list-mode'.")

(defconst exercism-track-list-title "Exercism Tracks"
  "Title shown in the track list buffer.")

(defvar-local exercism-track-list-tracks nil
  "Cached track plists for the current track list buffer.")

(defvar-local exercism-track-list-on-select nil
  "Callback invoked with a track slug when a track is selected.")

(defvar-local exercism-track-list-origin-buffer nil
  "Buffer from which the current track picker was opened.")

(defvar-local exercism-track-list-origin-window nil
  "Window from which the current track picker was opened.")

(defvar-local exercism-track-list-auth-present-p nil
  "Non-nil when the track list was loaded with authentication.")

(defun exercism-track-list--track-line-p ()
  "Return non-nil when point is on a track row."
  (get-text-property (point) 'exercism-track-slug))

(defun exercism-track-list--slug-at-point ()
  "Return the track slug at point, or nil when not on a track row."
  (and (exercism-track-list--track-line-p)
       (get-text-property (point) 'exercism-track-slug)))

(defun exercism-track-list--track-at-point ()
  "Return the cached track plist at point, or nil when not on a track row."
  (when-let ((slug (exercism-track-list--slug-at-point)))
    (seq-find (lambda (track)
                (equal (exercism--json-value (exercism--plist-get track 'slug))
                       slug))
              exercism-track-list-tracks)))

(defun exercism--track-joined-p (track)
  "Return non-nil when TRACK is joined on Exercism."
  (exercism--json-bool (exercism--plist-get track 'is_joined)))

(defun exercism--track-web-url (track)
  "Return the Exercism.org URL for TRACK."
  (or (when-let ((web-url (exercism--plist-get track 'web_url)))
        (exercism--json-value web-url))
      (format "https://exercism.org/tracks/%s"
              (exercism--json-value (exercism--plist-get track 'slug)))))

(defun exercism-track-list--track-buffer-live-p (buffer)
  "Return non-nil when BUFFER is a live Exercism track list buffer."
  (and buffer (buffer-live-p buffer)
       (with-current-buffer buffer
         (derived-mode-p 'exercism-track-list-mode))))

(defun exercism-track-list--find-track-by-slug (tracks slug)
  "Return the track plist for SLUG in TRACKS, or nil."
  (seq-find (lambda (track)
              (equal (exercism--json-value (exercism--plist-get track 'slug))
                     slug))
            tracks))

(defun exercism-track-list--restore-origin-window (track-buffer)
  "Show the track picker's origin buffer in TRACK-BUFFER's origin window."
  (with-current-buffer track-buffer
    (let ((origin-buffer exercism-track-list-origin-buffer)
          (origin-window exercism-track-list-origin-window))
      (when (and (window-live-p origin-window)
                 (buffer-live-p origin-buffer))
        (select-window origin-window)
        (switch-to-buffer origin-buffer)))))

(defun exercism-track-list--restore-origin (origin-window origin-buffer)
  "Select ORIGIN-WINDOW and display ORIGIN-BUFFER when both are live."
  (when (and (window-live-p origin-window)
             (buffer-live-p origin-buffer))
    (select-window origin-window)
    (switch-to-buffer origin-buffer)))

(defun exercism-track-list--complete-selection (track buffer)
  "Invoke the on-select callback for TRACK in its origin and close BUFFER."
  (let* ((slug (exercism--json-value (exercism--plist-get track 'slug)))
         (picker-state
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (list exercism-track-list-on-select
                    exercism-track-list-origin-buffer
                    exercism-track-list-origin-window))))
         (callback (car picker-state))
         (origin-buffer (cadr picker-state))
         (origin-window (caddr picker-state)))
    (unless (buffer-live-p origin-buffer)
      (user-error "The originating Exercism exercise list buffer no longer exists"))
    (when callback
      (with-current-buffer origin-buffer
        (funcall callback slug)))
    (exercism-track-list--restore-origin origin-window origin-buffer)
    (when (and buffer (buffer-live-p buffer) (not (eq buffer origin-buffer)))
      (kill-buffer buffer))))

(defun exercism-track-list-cancel ()
  "Cancel track selection and return to the exercise list."
  (interactive)
  (let ((track-buffer (current-buffer)))
    (if (with-current-buffer track-buffer exercism-track-list-origin-buffer)
        (progn
          (exercism-track-list--restore-origin-window track-buffer)
          (kill-buffer track-buffer))
      (quit-window))))

(defun exercism-track-list--refresh-and-verify-join (track buffer)
  "Refetch tracks and select TRACK in BUFFER if enrollment is detected."
  (let ((slug (exercism--json-value (exercism--plist-get track 'slug)))
        (title (exercism--json-value (exercism--plist-get track 'title))))
    (exercism--list-tracks
     (lambda (tracks)
       (when (exercism-track-list--track-buffer-live-p buffer)
         (with-current-buffer buffer
           (setq exercism-track-list-tracks tracks)
           (exercism--render-track-list)
           (let ((refreshed (exercism-track-list--find-track-by-slug tracks slug)))
             (if (and refreshed (exercism--track-joined-p refreshed))
                 (exercism-track-list--complete-selection refreshed buffer)
               (message "[exercism] enrollment was not detected for %s"
                        title))))))
     (cl-function
      (lambda (&key error-thrown response &allow-other-keys)
        (user-error "Failed to refresh tracks: %s"
                    (or (when response
                          (format "HTTP %s"
                                  (request-response-status-code response)))
                        (format "%s" error-thrown))))))))

(defun exercism-track-list--join-track-in-browser (track buffer)
  "Open TRACK in the browser and verify enrollment before selection."
  (browse-url (exercism--track-web-url track))
  (let ((title (exercism--json-value (exercism--plist-get track 'title))))
    (when (y-or-n-p (format "Joined %s in the browser? " title))
      (exercism-track-list--refresh-and-verify-join track buffer))))

(defun exercism-track-list--goto-next-slug ()
  "Move point to the next track row, if any."
  (let ((next (next-single-property-change (point) 'exercism-track-slug)))
    (when next
      (goto-char next)
      (unless (exercism-track-list--track-line-p)
        (exercism-track-list--goto-next-slug)))))

(defun exercism-track-list--goto-previous-slug ()
  "Move point to the previous track row, if any."
  (let ((prev (previous-single-property-change (point) 'exercism-track-slug)))
    (when prev
      (goto-char prev)
      (unless (exercism-track-list--track-line-p)
        (exercism-track-list--goto-previous-slug)))))

(defun exercism-track-list-next ()
  "Move to the next track row."
  (interactive)
  (exercism-track-list--goto-next-slug))

(defun exercism-track-list-previous ()
  "Move to the previous track row."
  (interactive)
  (exercism-track-list--goto-previous-slug))

(defun exercism-track-list-select-track ()
  "Select or join the track on the current line."
  (interactive)
  (let ((track (exercism-track-list--track-at-point))
        (buf (current-buffer)))
    (unless track
      (user-error "Not on a track row"))
    (unless exercism-track-list-auth-present-p
      (user-error "Configure Exercism first (`M-x exercism-configure`)"))
    (if (exercism--track-joined-p track)
        (exercism-track-list--complete-selection track buf)
      (exercism-track-list--join-track-in-browser track buf))))

(defun exercism-track-list-reload ()
  "Reload the track list in the current buffer."
  (interactive)
  (unless (derived-mode-p 'exercism-track-list-mode)
    (user-error "Not in Exercism track list buffer"))
  (exercism--list-tracks
   (lambda (tracks)
     (setq exercism-track-list-tracks tracks
           exercism-track-list-auth-present-p
           (not (null (exercism--maybe-get-api-token))))
     (exercism--render-track-list)
     (exercism--prefetch-track-icons tracks))))

(define-derived-mode exercism-track-list-mode special-mode "Exercism Tracks"
  "Major mode for browsing Exercism tracks."
  (setq buffer-read-only t)
  (hl-line-mode 1))

(defun exercism--track-list-longest (tracks property)
  "Return the longest PROPERTY string length among TRACKS."
  (apply #'max 0
         (mapcar (lambda (track)
                   (length (exercism--json-value
                            (exercism--plist-get track property))))
                 tracks)))

(defun exercism--track-list-column-widths (tracks auth-present-p)
  "Return column widths for TRACKS rendered with AUTH-PRESENT-P."
  (let ((enrollment-width
         (apply #'max 10 (mapcar (lambda (track)
                                   (exercism--track-list-label-width
                                    (exercism--track-list-enrollment-label
                                     (exercism--plist-get track 'is_joined)
                                     auth-present-p)))
                                 tracks)))
        (concepts-width
         (apply #'max 8 (mapcar (lambda (track)
                                  (exercism--track-list-label-width
                                   (exercism--track-list-concepts-label
                                    (exercism--plist-get track 'num_learnt_concepts)
                                    (exercism--plist-get track 'num_concepts)
                                    (exercism--track-list-show-progress-p
                                     auth-present-p
                                     (exercism--plist-get track 'is_joined)))))
                                tracks)))
        (exercises-width
         (apply #'max 9 (mapcar (lambda (track)
                                  (exercism--track-list-label-width
                                   (exercism--track-list-progress-label
                                    (exercism--plist-get track 'num_completed_exercises)
                                    (exercism--plist-get track 'num_exercises)
                                    (exercism--track-list-show-progress-p
                                     auth-present-p
                                     (exercism--plist-get track 'is_joined)))))
                                tracks)))
        (type-width
         (apply #'max 8 (mapcar (lambda (track)
                                  (exercism--track-list-label-width
                                   (exercism--track-list-type-label
                                    (exercism--plist-get track 'course))))
                                tracks)))
        (new-width 3)
        (notify-width
         (apply #'max 6 (mapcar (lambda (track)
                                  (exercism--track-list-label-width
                                   (exercism--track-list-notifications-label
                                    (exercism--plist-get track 'has_notifications)
                                    auth-present-p)))
                                tracks)))
        (touched-width
         (apply #'max 10 (mapcar (lambda (track)
                                   (exercism--track-list-label-width
                                    (exercism--track-list-last-touched-label
                                     (exercism--plist-get track 'last_touched_at))))
                                 tracks))))
    (list (exercism--track-list-longest tracks 'title)
          enrollment-width concepts-width exercises-width type-width
          new-width notify-width touched-width)))

(defun exercism--render-track-list ()
  "Redraw the track list buffer from cached data."
  (let* ((tracks exercism-track-list-tracks)
         (auth-present-p exercism-track-list-auth-present-p)
         (widths (exercism--track-list-column-widths tracks auth-present-p))
         (title-width (nth 0 widths))
         (enrollment-width (nth 1 widths))
         (concepts-width (nth 2 widths))
         (exercises-width (nth 3 widths))
         (type-width (nth 4 widths))
         (new-width (nth 5 widths))
         (notify-width (nth 6 widths))
         (touched-width (nth 7 widths))
         (title exercism-track-list-title))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert title "\n")
      (insert (make-string (length title) ?=) "\n\n")
      (insert "RET select/join | n/p move | g reload | q cancel\n\n")
      (insert (format "Tracks: %d\n\n" (length tracks)))
      (insert (exercism--track-icon-separator)
              (format (format "%%-%ds  %%-%ds  %%%ds  %%%ds  %%-%ds  %%-%ds  %%-%ds  %%-%ds\n"
                              title-width enrollment-width concepts-width
                              exercises-width type-width new-width
                              notify-width touched-width)
                      "Track" "Enrollment" "Concepts" "Exercises"
                      "Type" "New" "Notify" "Last touched"))
      (insert (make-string (+ title-width enrollment-width concepts-width
                              exercises-width type-width new-width
                              notify-width touched-width 18)
                           ?-)
              "\n")
      (seq-doseq (track tracks)
        (let* ((slug (exercism--json-value (exercism--plist-get track 'slug)))
               (track-title (exercism--json-value (exercism--plist-get track 'title)))
               (enrollment (exercism--track-list-enrollment-label
                            (exercism--plist-get track 'is_joined)
                            auth-present-p))
               (show-progress-p
                (exercism--track-list-show-progress-p
                 auth-present-p (exercism--plist-get track 'is_joined)))
               (concepts (exercism--track-list-concepts-label
                          (exercism--plist-get track 'num_learnt_concepts)
                          (exercism--plist-get track 'num_concepts)
                          show-progress-p))
               (exercises (exercism--track-list-progress-label
                           (exercism--plist-get track 'num_completed_exercises)
                           (exercism--plist-get track 'num_exercises)
                           show-progress-p))
               (type-label (exercism--track-list-type-label
                            (exercism--plist-get track 'course)))
               (new-label (exercism--track-list-is-new-label
                           (exercism--plist-get track 'is_new)))
               (notify-label (exercism--track-list-notifications-label
                              (exercism--plist-get track 'has_notifications)
                              auth-present-p))
               (touched-label (exercism--track-list-last-touched-label
                               (exercism--plist-get track 'last_touched_at)))
               (line-start (point))
               (row-face (when (equal slug exercism--current-track)
                           '(:weight bold))))
          (insert (exercism--track-icon-display slug)
                  (exercism--track-icon-separator)
                  (propertize (format (format "%%-%ds" title-width) track-title)
                              'face row-face)
                  "  "
                  (exercism--track-list-pad-label enrollment enrollment-width)
                  "  "
                  (exercism--track-list-pad-right concepts concepts-width)
                  "  "
                  (exercism--track-list-pad-right exercises exercises-width)
                  "  "
                  (format (format "%%-%ds" type-width) type-label)
                  "  "
                  (exercism--track-list-pad-label new-label new-width)
                  "  "
                  (exercism--track-list-pad-label notify-label notify-width)
                  "  "
                  (propertize (format (format "%%-%ds" touched-width) touched-label)
                              'face row-face)
                  "\n")
          (add-text-properties line-start (point)
                               `(exercism-track-slug ,slug))))
      (goto-char (point-min))
      (catch 'found
        (while (not (eobp))
          (when (get-text-property (point) 'exercism-track-slug)
            (throw 'found t))
          (forward-line 1))))))

(defun exercism--show-track-list (tracks on-select)
  "Cache TRACKS, render the picker, and call ON-SELECT with chosen slug."
  (let ((origin-buffer (current-buffer))
        (origin-window (selected-window))
        (track-buffer (get-buffer-create exercism--track-list-buffer-name)))
    (with-current-buffer track-buffer
      (exercism-track-list-mode)
      (setq exercism-track-list-tracks tracks
            exercism-track-list-on-select on-select
            exercism-track-list-origin-buffer origin-buffer
            exercism-track-list-origin-window origin-window
            exercism-track-list-auth-present-p
            (not (null (exercism--maybe-get-api-token))))
      (exercism--render-track-list)
      (exercism--prefetch-track-icons tracks)
      (with-selected-window origin-window
        (switch-to-buffer track-buffer)))))

(defun exercism--apply-track-selection (track on-ready)
  "Ensure TRACK exists locally, then call ON-READY with TRACK."
  (let ((track-dir (expand-file-name track exercism--workspace)))
    (if (file-exists-p track-dir)
        (funcall on-ready track)
      (exercism--track-init
       track
       (lambda (_result)
         (funcall on-ready track))))))

(defun exercism--prompt-for-track (on-select)
  "Fetch tracks, show the picker, and call ON-SELECT with the chosen slug."
  (exercism--list-tracks
   (lambda (tracks)
     (exercism--sync-workspace-from-config)
     (exercism--show-track-list tracks on-select))))

(defun exercism-exercise-list-set-track ()
  "Set the current Exercism track and reload the exercise list."
  (interactive)
  (unless (derived-mode-p 'exercism-exercise-list-mode)
    (user-error "Not in Exercism exercise list buffer"))
  (let ((exercise-list-buffer (current-buffer)))
    (exercism--prompt-for-track
     (lambda (track)
       (exercism--apply-track-selection
        track
        (lambda (_track)
          (exercism--exercise-list-apply-track-in-buffer
           track exercise-list-buffer)))))))

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

(defun exercism--show-exercise-list (exercises solution-status-by-slug &optional display-p)
  "Cache EXERCISES and SOLUTION-STATUS-BY-SLUG, then render the list.
Pass DISPLAY-P as `no-display' to re-render without changing windows."
  (with-current-buffer (get-buffer-create exercism--exercise-list-buffer-name)
    (exercism-exercise-list-mode)
    (setq exercism-exercise-list-exercises exercises
          exercism-exercise-list-solution-status-by-slug solution-status-by-slug)
    (exercism--render-exercise-list)
    (unless (eq display-p 'no-display)
      (pop-to-buffer (current-buffer)))))

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

(defun exercism--open-exercise-list ()
  "Fetch exercises for the current track and show the exercise list."
  (exercism--with-track-exercises-and-solutions
   (lambda (exercises solution-status-by-slug)
     (exercism--show-exercise-list exercises solution-status-by-slug))))

(defun exercism--select-track-and-open-exercises (track)
  "Set TRACK as current and open the exercise list."
  (setq exercism--current-track track)
  (exercism--save-state)
  (message "[exercism] set current track to: %s" track)
  (exercism--open-exercise-list))

(defun exercism ()
  "Open the Exercism exercise list for the current track."
  (interactive)
  (cond
   ((not (exercism--setup-ok-p))
    (message "[exercism] setup incomplete — running self-check")
    (exercism-self-check))
   (exercism--current-track
    (exercism--open-exercise-list))
   (t
    (exercism--prompt-for-track
     (lambda (track)
       (exercism--apply-track-selection
        track #'exercism--select-track-and-open-exercises))))))

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

(defun exercism--json-bool (value)
  "Return JSON boolean VALUE as an Emacs boolean."
  (pcase value
    (:json-false nil)
    (:json-true t)
    ((guard (memq value '(nil :null))) nil)
    (_ (not (null value)))))

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

(defun exercism--cli-version-self-check-result ()
  "Return a self-check result for the installed CLI version."
  (let ((label (format "CLI version (min %s)" exercism--min-cli-version))
        (exe (or (executable-find exercism-executable)
                 (when (file-executable-p exercism-executable)
                   exercism-executable))))
    (if (not exe)
        (list label nil "executable not found")
      (let ((output (shell-command-to-string
                      (concat (shell-quote-argument exercism-executable)
                              " version"))))
        (if (string-match "exercism version \\([0-9.]+\\)" output)
            (let ((version (match-string 1 output)))
              (if (exercism--compare-semvers version #'< exercism--min-cli-version)
                  (list label nil
                        (format "%s (below min %s)" version exercism--min-cli-version))
                (list label t version)))
          (list label nil (string-trim output)))))))

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
  (let* ((exe (or (executable-find exercism-executable)
                  (when (file-executable-p exercism-executable)
                    exercism-executable)))
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

(defun exercism--self-check-show-pending (status)
  "Show STATUS as the interim self-check report while async work runs."
  (with-current-buffer (exercism--self-check-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Exercism Self-Check\n")
      (insert "===================\n\n")
      (insert (exercism--self-check-key-help))
      (insert "\n\n")
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
        (insert "Exercism Self-Check\n")
        (insert "===================\n\n")
        (insert (exercism--self-check-key-help))
        (insert "\n\n")
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
        (setq exercism--current-track selected-track)
        (exercism--save-state)
        (message "[exercism] set current track to: %s" selected-track)
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

(defun exercism-self-check ()
  "Verify Exercism CLI setup and API connectivity, then show a report."
  (interactive)
  (setq exercism--self-check-results nil
        exercism--self-check-pending 0)
  (pop-to-buffer (exercism--self-check-buffer))
  (exercism--self-check-render)
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
      (exercism--self-check-add "Current exercise" t exercism--current-exercise)))
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
     (cond
      ((not version)
       (message "[exercism] error: could not determine CLI version"))
      ((exercism--compare-semvers version #'< exercism--min-cli-version)
       (message "[exercism] error: running tests requires CLI %s+ (you have %s)"
                exercism--min-cli-version version))
      (t
       (let* ((default-directory exercise-dir)
              (compile-command (concat (shell-quote-argument exercism-executable)
                                       " test")))
         (compile compile-command)))))))

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
