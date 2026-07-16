;;; exercism-track-list.el --- Track list UI for exercism.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Track icon cache, labels, buffer mode, rendering, enrollment, and
;; selection flow for the Exercism track picker.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'exercism-core)
(require 'exercism-cli)
(require 'exercism-api)
(require 'exercism-list)

;;;; Track icons

(defvar exercism--track-icon-size 16
  "Height and width in pixels for track icons in the track list.")

(defvar exercism--track-icon-cache-root nil
  "When non-nil, override the root directory for cached track icons.")

(defvar exercism--track-list-buffer-name "*Exercism Tracks*"
  "Buffer name for track listings.")

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

(defun exercism--track-icon-save-http-body (path)
  "Save an SVG HTTP body from the current buffer to PATH when present."
  (goto-char (point-min))
  (when (re-search-forward "\r?\n\r?\n" nil t)
    (let ((data (buffer-substring-no-properties (point) (point-max))))
      (when (string-match-p "\\`\\s-*<svg" data)
        (with-temp-file path
          (insert data))))))

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
                 (exercism--track-icon-save-http-body path))
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

;;;; Labels and padding

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

;;;; Mode and buffer state

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

(define-derived-mode exercism-track-list-mode special-mode "Exercism Tracks"
  "Major mode for browsing Exercism tracks."
  (setq buffer-read-only t)
  (hl-line-mode 1))

;;;; Row navigation and lookup

(defun exercism-track-list--track-line-p ()
  "Return non-nil when point is on a track row."
  (exercism--list-row-p 'exercism-track-slug))

(defun exercism-track-list--slug-at-point ()
  "Return the track slug at point, or nil when not on a track row."
  (exercism--list-slug-at-point 'exercism-track-slug))

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

(defun exercism-track-list--goto-next-slug ()
  "Move point to the next track row, if any."
  (exercism--list-goto-next-row 'exercism-track-slug))

(defun exercism-track-list--goto-previous-slug ()
  "Move point to the previous track row, if any."
  (exercism--list-goto-previous-row 'exercism-track-slug))

(defun exercism-track-list-next ()
  "Move to the next track row."
  (interactive)
  (exercism-track-list--goto-next-slug))

(defun exercism-track-list-previous ()
  "Move to the previous track row."
  (interactive)
  (exercism-track-list--goto-previous-slug))

;;;; Selection and enrollment

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

(defun exercism-track-list--capture-picker-state (buffer)
  "Return (CALLBACK ORIGIN-BUFFER ORIGIN-WINDOW) from BUFFER, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (list exercism-track-list-on-select
            exercism-track-list-origin-buffer
            exercism-track-list-origin-window))))

(defun exercism-track-list--invoke-callback (callback origin-buffer slug)
  "Call CALLBACK with SLUG in ORIGIN-BUFFER when CALLBACK is non-nil."
  (when callback
    (with-current-buffer origin-buffer
      (funcall callback slug))))

(defun exercism-track-list--close-picker-buffer (buffer origin-buffer)
  "Kill BUFFER when it is live and distinct from ORIGIN-BUFFER."
  (when (and buffer (buffer-live-p buffer) (not (eq buffer origin-buffer)))
    (kill-buffer buffer)))

(defun exercism-track-list--complete-selection (track buffer)
  "Invoke the on-select callback for TRACK in its origin and close BUFFER."
  (let* ((slug (exercism--json-value (exercism--plist-get track 'slug)))
         (picker-state (exercism-track-list--capture-picker-state buffer))
         (callback (car picker-state))
         (origin-buffer (cadr picker-state))
         (origin-window (caddr picker-state)))
    (unless (buffer-live-p origin-buffer)
      (user-error "The originating Exercism exercise list buffer no longer exists"))
    (exercism-track-list--invoke-callback callback origin-buffer slug)
    (exercism-track-list--restore-origin origin-window origin-buffer)
    (exercism-track-list--close-picker-buffer buffer origin-buffer)))

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
                    (exercism--request-error-message error-thrown response)))))))

(defun exercism-track-list--join-track-in-browser (track buffer)
  "Open TRACK in the browser and verify enrollment before selection."
  (browse-url (exercism--track-web-url track))
  (let ((title (exercism--json-value (exercism--plist-get track 'title))))
    (when (y-or-n-p (format "Joined %s in the browser? " title))
      (exercism-track-list--refresh-and-verify-join track buffer))))

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

;;;; Rendering

(defun exercism--track-list-longest (tracks property)
  "Return the longest PROPERTY string length among TRACKS."
  (exercism--list-longest-field tracks property))

(defun exercism--track-list-max-label-width (tracks min-width label-fn)
  "Return the max of MIN-WIDTH and LABEL-FN widths across TRACKS."
  (apply #'max min-width
         (mapcar (lambda (track)
                   (exercism--track-list-label-width (funcall label-fn track)))
                 tracks)))

(defun exercism--track-list-column-widths (tracks auth-present-p)
  "Return column widths for TRACKS rendered with AUTH-PRESENT-P."
  (list
   (exercism--list-longest-field tracks 'title)
   (exercism--track-list-max-label-width
    tracks 10
    (lambda (track)
      (exercism--track-list-enrollment-label
       (exercism--plist-get track 'is_joined)
       auth-present-p)))
   (exercism--track-list-max-label-width
    tracks 8
    (lambda (track)
      (exercism--track-list-concepts-label
       (exercism--plist-get track 'num_learnt_concepts)
       (exercism--plist-get track 'num_concepts)
       (exercism--track-list-show-progress-p
        auth-present-p
        (exercism--plist-get track 'is_joined)))))
   (exercism--track-list-max-label-width
    tracks 9
    (lambda (track)
      (exercism--track-list-progress-label
       (exercism--plist-get track 'num_completed_exercises)
       (exercism--plist-get track 'num_exercises)
       (exercism--track-list-show-progress-p
        auth-present-p
        (exercism--plist-get track 'is_joined)))))
   (exercism--track-list-max-label-width
    tracks 8
    (lambda (track)
      (exercism--track-list-type-label
       (exercism--plist-get track 'course))))
   3
   (exercism--track-list-max-label-width
    tracks 6
    (lambda (track)
      (exercism--track-list-notifications-label
       (exercism--plist-get track 'has_notifications)
       auth-present-p)))
   (exercism--track-list-max-label-width
    tracks 10
    (lambda (track)
      (exercism--track-list-last-touched-label
       (exercism--plist-get track 'last_touched_at))))))

(defun exercism--track-list-insert-heading ()
  "Insert the track list title and key help."
  (exercism--list-insert-heading
   exercism-track-list-title
   "RET select/join | n/p move | g reload | q cancel"))

(defun exercism--track-list-insert-summary (tracks)
  "Insert the track count summary for TRACKS."
  (insert (format "Tracks: %d\n\n" (length tracks))))

(defun exercism--track-list-insert-column-header (widths)
  "Insert the track list column header and separator for WIDTHS."
  (let ((title-width (nth 0 widths))
        (enrollment-width (nth 1 widths))
        (concepts-width (nth 2 widths))
        (exercises-width (nth 3 widths))
        (type-width (nth 4 widths))
        (new-width (nth 5 widths))
        (notify-width (nth 6 widths))
        (touched-width (nth 7 widths)))
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
            "\n")))

(defun exercism--track-list-row-presentation (track auth-present-p)
  "Return display fields for TRACK given AUTH-PRESENT-P."
  (let* ((slug (exercism--json-value (exercism--plist-get track 'slug)))
         (show-progress-p
          (exercism--track-list-show-progress-p
           auth-present-p (exercism--plist-get track 'is_joined))))
    (list :slug slug
          :title (exercism--json-value (exercism--plist-get track 'title))
          :enrollment (exercism--track-list-enrollment-label
                       (exercism--plist-get track 'is_joined)
                       auth-present-p)
          :concepts (exercism--track-list-concepts-label
                     (exercism--plist-get track 'num_learnt_concepts)
                     (exercism--plist-get track 'num_concepts)
                     show-progress-p)
          :exercises (exercism--track-list-progress-label
                      (exercism--plist-get track 'num_completed_exercises)
                      (exercism--plist-get track 'num_exercises)
                      show-progress-p)
          :type (exercism--track-list-type-label
                 (exercism--plist-get track 'course))
          :new (exercism--track-list-is-new-label
                (exercism--plist-get track 'is_new))
          :notify (exercism--track-list-notifications-label
                   (exercism--plist-get track 'has_notifications)
                   auth-present-p)
          :touched (exercism--track-list-last-touched-label
                    (exercism--plist-get track 'last_touched_at))
          :face (when (equal slug exercism--current-track)
                  '(:weight bold)))))

(defun exercism--track-list-insert-row (presentation widths)
  "Insert one track row from PRESENTATION using WIDTHS."
  (let* ((slug (plist-get presentation :slug))
         (title-width (nth 0 widths))
         (enrollment-width (nth 1 widths))
         (concepts-width (nth 2 widths))
         (exercises-width (nth 3 widths))
         (type-width (nth 4 widths))
         (new-width (nth 5 widths))
         (notify-width (nth 6 widths))
         (touched-width (nth 7 widths))
         (row-face (plist-get presentation :face))
         (line-start (point)))
    (insert (exercism--track-icon-display slug)
            (exercism--track-icon-separator)
            (propertize (format (format "%%-%ds" title-width)
                                (plist-get presentation :title))
                        'face row-face)
            "  "
            (exercism--track-list-pad-label
             (plist-get presentation :enrollment) enrollment-width)
            "  "
            (exercism--track-list-pad-right
             (plist-get presentation :concepts) concepts-width)
            "  "
            (exercism--track-list-pad-right
             (plist-get presentation :exercises) exercises-width)
            "  "
            (format (format "%%-%ds" type-width)
                    (plist-get presentation :type))
            "  "
            (exercism--track-list-pad-label
             (plist-get presentation :new) new-width)
            "  "
            (exercism--track-list-pad-label
             (plist-get presentation :notify) notify-width)
            "  "
            (propertize (format (format "%%-%ds" touched-width)
                                (plist-get presentation :touched))
                        'face row-face)
            "\n")
    (add-text-properties line-start (point)
                         `(exercism-track-slug ,slug))))

(defun exercism--track-list-insert-rows (tracks auth-present-p widths)
  "Insert all TRACKS rows for AUTH-PRESENT-P using WIDTHS."
  (seq-doseq (track tracks)
    (exercism--track-list-insert-row
     (exercism--track-list-row-presentation track auth-present-p)
     widths)))

(defun exercism--render-track-list ()
  "Redraw the track list buffer from cached data."
  (let* ((tracks exercism-track-list-tracks)
         (auth-present-p exercism-track-list-auth-present-p)
         (widths (exercism--track-list-column-widths tracks auth-present-p)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (exercism--track-list-insert-heading)
      (exercism--track-list-insert-summary tracks)
      (exercism--track-list-insert-column-header widths)
      (exercism--track-list-insert-rows tracks auth-present-p widths)
      (exercism--list-goto-first-row 'exercism-track-slug))))

;;;; Entry points

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

(provide 'exercism-track-list)
;;; exercism-track-list.el ends here
