;;; exercism-exercise-list.el --- Exercise list UI for exercism.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercise list buffer, pending submit animation, open/submit/test
;; commands, and download helpers for Exercism exercises.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'exercism-core)
(require 'exercism-cli)
(require 'exercism-api)
(require 'exercism-list)
(require 'exercism-track-list)

(declare-function exercism-self-check "exercism-self-check")

;;;; State helpers

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

;;;; Buffer, mode, keymap

(defvar exercism--exercise-list-buffer-name "*Exercism Exercises*"
  "Buffer name for exercise listings.")

(defvar exercism-exercise-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'exercism-exercise-list-open-exercise)
    (define-key map (kbd "n") #'exercism-exercise-list-next)
    (define-key map (kbd "p") #'exercism-exercise-list-previous)
    (define-key map (kbd "g") #'exercism-exercise-list-reload)
    (define-key map (kbd "d") #'exercism-exercise-list-download-exercise)
    (define-key map (kbd "D") #'exercism-download-all-unlocked-exercises)
    (define-key map (kbd "t") #'exercism-exercise-list-set-track)
    (define-key map (kbd "c") #'exercism-configure)
    (define-key map (kbd "C") #'exercism-self-check)
    (define-key map (kbd "s") #'exercism-exercise-list-submit-exercise)
    (define-key map (kbd "r") #'exercism-exercise-list-run-tests)
    (define-key map (kbd "b") #'exercism-exercise-list-open-in-browser)
    (define-key map (kbd "?") #'exercism-exercise-list-show-help)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `exercism-exercise-list-mode'.")

(defconst exercism-exercise-list-title "Exercism Exercises"
  "Title shown in the exercise list buffer.")

(defconst exercism-exercise-list-key-help
  "RET open | s submit | r test | b browser | d download | D download all | t track"
  "Short key help shown in the exercise list heading.")

(defconst exercism-exercise-list-full-key-help
  "\
Exercism Exercises — keys

RET  Open exercise (download if needed)
s    Submit
r    Run tests
b    Open in browser
d    Download current
D    Download all unlocked
t    Track picker

n/p  Next / previous
g    Reload
c    Configure
C    Self-check
q    Quit

?    This help"
  "Full key help shown by `exercism-exercise-list-show-help'.")

(defvar exercism--exercise-list-help-buffer-name "*Exercism Exercises Help*"
  "Buffer name for exercise list key help.")

(defvar exercism-exercise-list-help-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `exercism-exercise-list-help-mode'.")

(define-derived-mode exercism-exercise-list-help-mode special-mode
  "Exercism Exercises Help"
  "Major mode for the Exercism exercise list key help buffer."
  (setq buffer-read-only t))

(defun exercism-exercise-list-show-help ()
  "Show the full exercise list keybinding help in a buffer."
  (interactive)
  (let ((buf (get-buffer-create exercism--exercise-list-help-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert exercism-exercise-list-full-key-help)
        (insert "\n")
        (goto-char (point-min)))
      (exercism-exercise-list-help-mode))
    (pop-to-buffer buf)))

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

;;;; Navigation

(defun exercism-exercise-list--exercise-line-p ()
  "Return non-nil when point is on an exercise row."
  (exercism--list-row-p 'exercism-exercise-slug))

(defun exercism-exercise-list--slug-at-point ()
  "Return the exercise slug at point, or nil when not on an exercise row."
  (exercism--list-slug-at-point 'exercism-exercise-slug))

(defun exercism-exercise-list--goto-next-slug ()
  "Move point to the next exercise row, if any."
  (exercism--list-goto-next-row 'exercism-exercise-slug))

(defun exercism-exercise-list--goto-previous-slug ()
  "Move point to the previous exercise row, if any."
  (exercism--list-goto-previous-row 'exercism-exercise-slug))

(defun exercism-exercise-list--require-unlocked-slug-at-point ()
  "Return the unlocked exercise slug at point, or signal `user-error'."
  (let ((slug (exercism-exercise-list--slug-at-point))
        (unlocked-p (get-text-property (point) 'exercism-exercise-unlocked)))
    (unless slug
      (user-error "Not on an exercise row"))
    (unless unlocked-p
      (user-error "Exercise %s is locked" slug))
    slug))

(defun exercism-exercise-list-next ()
  "Move to the next exercise row."
  (interactive)
  (exercism-exercise-list--goto-next-slug))

(defun exercism-exercise-list-previous ()
  "Move to the previous exercise row."
  (interactive)
  (exercism-exercise-list--goto-previous-slug))

;;;; Commands

(defun exercism-exercise-list-open-exercise ()
  "Open the exercise on the current line, downloading if needed."
  (interactive)
  (exercism--open-exercise-slug
   (exercism-exercise-list--require-unlocked-slug-at-point)))

(defun exercism-exercise-list-download-exercise ()
  "Download the exercise on the current line (force if incomplete)."
  (interactive)
  (exercism--download-exercise-slug
   (exercism-exercise-list--require-unlocked-slug-at-point)))

(defun exercism-exercise-list-submit-exercise ()
  "Submit the exercise on the current line, after confirmation."
  (interactive)
  (let ((slug (exercism-exercise-list--require-unlocked-slug-at-point)))
    (when (y-or-n-p (format "Submit exercise %s on track %s? "
                            slug exercism--current-track))
      (exercism--submit-slug slug))))

(defun exercism-exercise-list-submit-then-open-in-browser ()
  "Submit the exercise on the current line, then open it in a browser."
  (interactive)
  (let ((slug (exercism-exercise-list--require-unlocked-slug-at-point)))
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
  (exercism--set-current-track track)
  (exercism-exercise-list-reload))

(defun exercism--exercise-list-apply-track-in-buffer (track buffer)
  "Apply TRACK in BUFFER when it is a live exercise list buffer."
  (unless (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (derived-mode-p 'exercism-exercise-list-mode)))
    (user-error "Buffer is not a live Exercism exercise list buffer"))
  (with-current-buffer buffer
    (exercism--exercise-list-apply-track track)))

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

;;;; Rendering

(defun exercism--exercise-list-longest (exercises property)
  "Return the longest PROPERTY string length among EXERCISES."
  (exercism--list-longest-field exercises property))

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
  (let (unsolved solved)
    (dolist (exercise exercises)
      (let ((slug (exercism--json-value
                   (exercism--plist-get exercise 'slug))))
        (if (exercism--exercise-list-solved-p
             (gethash slug solution-status-by-slug))
            (push exercise solved)
          (push exercise unsolved))))
    (nconc (nreverse unsolved) (nreverse solved))))

(defun exercism--exercise-list-insert-heading ()
  "Insert the exercise list title and key help."
  (exercism--list-insert-heading
   exercism-exercise-list-title
   exercism-exercise-list-key-help))

(defun exercism--ensure-current-track-icon ()
  "Fetch the current track icon when missing, then refresh the exercise list."
  (when-let ((slug exercism--current-track))
    (unless (exercism--svg-file-p (exercism--track-icon-cache-path slug))
      (exercism--fetch-track-icon
       slug
       (format "https://assets.exercism.org/tracks/%s.svg" slug)
       (lambda (path)
         (when path
           (when-let ((buffer (get-buffer exercism--exercise-list-buffer-name)))
             (with-current-buffer buffer
               (when (derived-mode-p 'exercism-exercise-list-mode)
                 (exercism--render-exercise-list))))))))))

(defun exercism--exercise-list-insert-summary (exercises counts)
  "Insert track and exercise count summary for EXERCISES using COUNTS."
  (let ((solved-count (car counts))
        (unsolved-count (cadr counts)))
    (insert (exercism--track-icon-display exercism--current-track)
            " "
            (format "Track: %s\n" exercism--current-track))
    (insert (format "Exercises: %d | Solved: %d | Unsolved: %d\n\n"
                    (length exercises)
                    solved-count unsolved-count))))

(defun exercism--exercise-list-insert-column-header (state-width difficulty-width slug-width)
  "Insert the exercise list column header and separator."
  (insert (format (format "%%-%ds  %%-%ds  %%-%ds  %%s\n"
                          state-width difficulty-width slug-width)
                  "Status" "Difficulty" "Exercise" "Blurb"))
  (insert (make-string (+ state-width difficulty-width slug-width 8) ?-) "\n"))

(defun exercism--exercise-list-difficulty-face (difficulty)
  "Return the face for DIFFICULTY."
  (pcase difficulty
    ("easy" '(:foreground "green"))
    ("medium" '(:foreground "yellow"))
    ("hard" '(:foreground "red"))
    (_ '(:foreground "blue"))))

(defun exercism--exercise-list-row-presentation (exercise solution-status)
  "Return display fields for EXERCISE given SOLUTION-STATUS."
  (let* ((slug (exercism--json-value (exercism--plist-get exercise 'slug)))
         (difficulty (exercism--json-value (exercism--plist-get exercise 'difficulty)))
         (blurb (exercism--plist-get exercise 'blurb))
         (state (exercism--exercise-list-state exercise solution-status))
         (unlocked-p (exercism--plist-get exercise 'is_unlocked)))
    (list :slug slug
          :difficulty difficulty
          :blurb blurb
          :state state
          :unlocked-p unlocked-p
          :difficulty-face (exercism--exercise-list-difficulty-face difficulty))))

(defun exercism--exercise-list-insert-row (presentation state-width difficulty-width slug-width)
  "Insert one exercise row from PRESENTATION using column widths."
  (let* ((slug (plist-get presentation :slug))
         (unlocked-p (plist-get presentation :unlocked-p))
         (line-start (point)))
    (insert (format (format "%%-%ds  " state-width)
                    (exercism--exercise-list-state-label
                     (plist-get presentation :state)))
            (propertize (format (format "%%-%ds" difficulty-width)
                                (plist-get presentation :difficulty))
                        'face (plist-get presentation :difficulty-face))
            "  "
            (format (format "%%-%ds" slug-width) slug)
            "  "
            (propertize (plist-get presentation :blurb) 'face 'shadow)
            "\n")
    (add-text-properties line-start (point)
                         `(exercism-exercise-slug ,slug
                           exercism-exercise-unlocked ,unlocked-p))))

(defun exercism--render-exercise-list ()
  "Redraw the exercise list buffer from cached data."
  (let* ((exercises exercism-exercise-list-exercises)
         (solution-status-by-slug exercism-exercise-list-solution-status-by-slug)
         (ordered (exercism--order-exercises
                   exercises solution-status-by-slug))
         (state-width 11)
         (difficulty-width (exercism--exercise-list-longest ordered 'difficulty))
         (slug-width (exercism--exercise-list-longest ordered 'slug))
         (counts (exercism--exercise-list-counts exercises solution-status-by-slug)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (exercism--exercise-list-insert-heading)
      (exercism--exercise-list-insert-summary exercises counts)
      (exercism--exercise-list-insert-column-header
       state-width difficulty-width slug-width)
      (seq-doseq (exercise ordered)
        (let ((slug (exercism--json-value (exercism--plist-get exercise 'slug))))
          (exercism--exercise-list-insert-row
           (exercism--exercise-list-row-presentation
            exercise (gethash slug solution-status-by-slug))
           state-width difficulty-width slug-width)))
      (setq exercism-exercise-list-state-width state-width)
      (exercism--list-goto-first-row 'exercism-exercise-slug)
      (exercism--ensure-current-track-icon))))

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
  (exercism--set-current-track track)
  (exercism--open-exercise-list))

;;;; Config, open, submit

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

(defun exercism--exercise-downloaded-p (exercise-dir)
  "Return non-nil when EXERCISE-DIR is a complete Exercism download."
  (when (file-directory-p exercise-dir)
    (let ((config-path (expand-file-name ".exercism/config.json" exercise-dir)))
      (when (file-readable-p config-path)
        (condition-case nil
            (let ((paths (exercism--solution-file-paths exercise-dir)))
              (and paths (seq-every-p #'file-exists-p paths)))
          (error nil))))))

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

(cl-defun exercism--submit-slug (slug &optional open-in-browser-after-p)
  "Submit SLUG on `exercism--current-track'."
  (exercism--ensure-current-track)
  (when (eq (gethash slug exercism--exercise-pending-states) 'submitting)
    (message "[exercism] already submitting %s" slug)
    (cl-return-from exercism--submit-slug))
  (let ((exercise-dir (exercism--exercise-dir-for-slug slug)))
    (unless (exercism--exercise-downloaded-p exercise-dir)
      (user-error "Exercise %s is not downloaded" slug))
    (let* ((solution-files (exercism--solution-file-paths exercise-dir))
           (default-directory exercise-dir)
           (submit-command (exercism--build-submit-command solution-files)))
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

(defun exercism--download-exercise-slug (slug)
  "Download SLUG on the current track if incomplete, forcing when the dir exists."
  (let ((exercise-dir (exercism--exercise-dir-for-slug slug)))
    (if (exercism--exercise-downloaded-p exercise-dir)
        (message "[exercism] %s already downloaded" slug)
      (message "[exercism] downloading %s exercise %s... (please wait)"
               exercism--current-track slug)
      (exercism--download-exercise
       slug exercism--current-track
       (lambda (exit-code result)
         (message "[exercism] download result: %s" result)
         (unless (exercism--download-succeeded-p exit-code result exercise-dir)
           (message "[exercism] download failed for %s: %s"
                    slug (string-trim result))))
       (file-directory-p exercise-dir)))))

(defun exercism--open-exercise-slug (slug)
  "Download SLUG on the current track if needed, then open it."
  (let ((exercise-dir (exercism--exercise-dir-for-slug slug)))
    (if (exercism--exercise-downloaded-p exercise-dir)
        (progn
          (exercism--open-exercise-dir exercise-dir)
          (setq exercism--current-exercise slug)
          (exercism--save-state))
      (message "[exercism] downloading %s exercise %s... (please wait)"
               exercism--current-track slug)
      (exercism--download-exercise
       slug exercism--current-track
       (lambda (exit-code result)
         (message "[exercism] download result: %s" result)
         (when (exercism--download-succeeded-p exit-code result exercise-dir)
           (exercism--open-exercise-dir exercise-dir)
           (setq exercism--current-exercise slug)
           (exercism--save-state)))
       (file-directory-p exercise-dir)))))

(defvar exercism--download-all-delay 1.0
  "Seconds to wait between successful download-all queue items.")

(defvar exercism--download-all-rate-limit-backoff 5.0
  "Seconds to wait before retrying a rate-limited download-all item.")

(defvar exercism--download-all-schedule-fn #'run-at-time
  "Scheduler used by download-all; tests may bind a synchronous function.")

(defun exercism--download-all-schedule (seconds function)
  "Run FUNCTION after SECONDS using `exercism--download-all-schedule-fn'."
  (funcall exercism--download-all-schedule-fn seconds nil function))

(defun exercism--download-all-queue (queue downloaded failed skipped &optional retrying)
  "Download QUEUE slugs sequentially, then report DOWNLOADED/FAILED/SKIPPED."
  (if (null queue)
      (message "[exercism] download-all finished: %d downloaded, %d skipped, %d failed"
               downloaded skipped failed)
    (let* ((slug (car queue))
           (rest (cdr queue))
           (exercise-dir (exercism--exercise-dir-for-slug slug))
           (force (file-directory-p exercise-dir)))
      (message "[exercism] attempting to download %s exercise %s..."
               exercism--current-track slug)
      (exercism--download-exercise
       slug exercism--current-track
       (lambda (exit-code result)
         (cond
          ((exercism--download-succeeded-p exit-code result exercise-dir)
           (exercism--download-all-schedule
            exercism--download-all-delay
            (lambda ()
              (exercism--download-all-queue rest (1+ downloaded) failed skipped))))
          ((and (not retrying) (exercism--cli-rate-limited-p result))
           (message "[exercism] rate limited on %s; retrying after backoff..." slug)
           (exercism--download-all-schedule
            exercism--download-all-rate-limit-backoff
            (lambda ()
              (exercism--download-all-queue queue downloaded failed skipped t))))
          (t
           (message "[exercism] download failed for %s: %s"
                    slug (string-trim result))
           (exercism--download-all-schedule
            exercism--download-all-delay
            (lambda ()
              (exercism--download-all-queue rest downloaded (1+ failed) skipped))))))
       force))))

(defun exercism-download-all-unlocked-exercises ()
  "Download all unlocked exercises for the current track."
  (interactive)
  (unless exercism--current-track
    (user-error "Set a track first (`t' in the exercise list, or `M-x exercism-exercise-list-set-track')"))
  (exercism--list-exercises
   exercism--current-track t
   (lambda (track-exercises)
     (let* ((slugs (mapcar (lambda (exercise)
                             (exercism--json-value
                              (exercism--plist-get exercise 'slug)))
                           track-exercises))
            (pending (seq-filter
                      (lambda (slug)
                        (not (exercism--exercise-downloaded-p
                              (exercism--exercise-dir-for-slug slug))))
                      slugs))
            (skipped (- (length slugs) (length pending))))
       (if (null pending)
           (message "[exercism] download-all finished: 0 downloaded, %d skipped, 0 failed"
                    skipped)
         (exercism--download-all-queue pending 0 0 skipped))))))

;;;; Tests

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
       (let ((default-directory exercise-dir)
             (compile-command (exercism--build-test-command)))
         (compile compile-command)))))))

(defun exercism--run-tests-for-slug (slug)
  "Run tests for SLUG on `exercism--current-track'."
  (exercism--ensure-current-track)
  (let ((exercise-dir (exercism--exercise-dir-for-slug slug)))
    (unless (exercism--exercise-downloaded-p exercise-dir)
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
  (exercism--run-tests-for-slug
   (exercism-exercise-list--require-unlocked-slug-at-point)))

(provide 'exercism-exercise-list)
;;; exercism-exercise-list.el ends here
