;;; exercism-ert.el --- ERT tests for exercism.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for pure helpers in `exercism.el'.
;; Run via ./scripts/run-exercism-ert.sh or M-x ert after loading this file.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'exercism)

(defun exercism-ert--sample-exercises ()
  "Return fixture exercise alists for tests."
  '(((slug . "hello-world")
     (difficulty . "easy")
     (blurb . "Say hi")
     (is_unlocked . t))
    ((slug . "two-fer")
     (difficulty . "medium")
     (blurb . "Share a cookie")
     (is_unlocked . t))
    ((slug . "secret-handshake")
     (difficulty . "hard")
     (blurb . "Shake hands")
     (is_unlocked . nil))
    ((slug . "bob")
     (difficulty . "easy")
     (blurb . "Bob says hi")
     (is_unlocked . t))))

(defun exercism-ert--make-solution-table (pairs)
  "Return a slug->status hash table from PAIRS alist."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair pairs)
      (puthash (car pair) (cdr pair) table))
    table))

(defun exercism-ert--label-text (label)
  "Return the display text of a propertized exercise list LABEL."
  (substring-no-properties label))

(defun exercism-ert--with-exercise-list (exercises solutions body)
  "Show exercise list in a temp buffer and run BODY there."
  (unwind-protect
      (let ((exercism--current-track "emacs-lisp"))
        (exercism--show-exercise-list exercises solutions)
        (with-current-buffer exercism--exercise-list-buffer-name
          (funcall body)))
    (when (get-buffer exercism--exercise-list-buffer-name)
      (kill-buffer exercism--exercise-list-buffer-name))))

(defun exercism-ert--goto-exercise-slug (slug)
  "Move point to the exercise row for SLUG in the current buffer."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (equal slug (get-text-property (point) 'exercism-exercise-slug))
        (throw 'found t))
      (forward-line 1))
    (error "Exercise row not found: %s" slug)))

(defun exercism-ert--exercise-slugs-in-buffer ()
  "Return exercise slugs in their displayed order."
  (let (slugs)
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((slug (get-text-property (point) 'exercism-exercise-slug)))
        (push slug slugs))
      (forward-line 1))
    (nreverse slugs)))

(defun exercism-ert--find-file-recorder (orig file &rest args)
  "Advice that records the file passed to `find-file'."
  (setq exercism-ert--find-file-target file)
  (apply orig file args))

(defvar exercism-ert--find-file-target nil)

(defun exercism-ert--download-exercise-recorder (orig exercise-slug track-slug callback)
  "Advice that records download args and simulates success."
  (setq exercism-ert--download-args (list exercise-slug track-slug))
  (let ((exercise-dir (expand-file-name exercise-slug
                                        (expand-file-name track-slug exercism--workspace))))
    (exercism-ert--write-minimal-exercise exercise-dir)
    (funcall callback "Downloaded")))

(defvar exercism-ert--download-args nil)

(defun exercism-ert--open-slug-recorder (orig slug)
  "Advice that records the slug passed to `exercism--open-exercise-slug'."
  (setq exercism-ert--opened-slug slug))

(defvar exercism-ert--opened-slug nil)

(defvar exercism-ert--submit-slug nil)

(defvar exercism-ert--submit-command nil)

(defvar exercism-ert--submit-sync-result "Submitted successfully")

(defun exercism-ert--submit-slug-recorder (orig slug &rest _args)
  "Advice that records the slug passed to `exercism--submit-slug'."
  (setq exercism-ert--submit-slug slug))

(defun exercism-ert--run-shell-command-recorder (orig shell-cmd &rest args)
  "Advice that records SHELL-CMD passed to `exercism--run-shell-command'."
  (setq exercism-ert--submit-command shell-cmd)
  (apply orig shell-cmd args))

(defun exercism-ert--run-shell-command-immediate (orig shell-cmd &optional callback)
  "Advice that invokes CALLBACK synchronously with `exercism-ert--submit-sync-result'."
  (setq exercism-ert--submit-command shell-cmd)
  (when callback
    (funcall callback exercism-ert--submit-sync-result)))

(defun exercism-ert--write-minimal-exercise (exercise-dir &optional solution-file)
  "Create a minimal Exercism exercise tree in EXERCISE-DIR."
  (let ((solution (or solution-file "solution.el"))
        (config-dir (expand-file-name ".exercism" exercise-dir)))
    (make-directory config-dir t)
    (write-region (format "{\"files\":{\"solution\":[\"%s\"]}}" solution)
                  nil (expand-file-name "config.json" config-dir))
    (write-region ";; solution\n" nil (expand-file-name solution exercise-dir))))

(defun exercism-ert--with-track-recorder (orig callback)
  "Advice that invokes CALLBACK with sample exercises and solutions."
  (funcall callback
           (exercism-ert--sample-exercises)
           (exercism-ert--make-solution-table
            '(("hello-world" . "published")
              ("two-fer" . "started")
              ("bob" . nil)))))

(defun exercism-ert--with-track-recorder-all-solved (orig callback)
  "Advice that invokes CALLBACK with all sample exercises solved."
  (funcall callback
           (exercism-ert--sample-exercises)
           (exercism-ert--make-solution-table
            '(("hello-world" . "published")
              ("two-fer" . "published")
              ("secret-handshake" . "published")
              ("bob" . "published")))))

(ert-deftest exercism--plist-get-symbol-key ()
  (should (equal "hello"
                 (exercism--plist-get '((slug . "hello")) 'slug))))

(ert-deftest exercism--plist-get-string-key ()
  (should (equal "hello"
                 (exercism--plist-get '(("slug" . "hello")) "slug"))))

(ert-deftest exercism--json-value-types ()
  (should (equal "easy" (exercism--json-value "easy")))
  (should (equal "published" (exercism--json-value 'published)))
  (should (equal "42" (exercism--json-value 42))))

(ert-deftest exercism--cli-error-p ()
  (should (exercism--cli-error-p "Error: token invalid"))
  (should (exercism--cli-error-p "  Error: missing track"))
  (should (not (exercism--cli-error-p "Downloaded exercise"))))

(ert-deftest exercism--cli-already-exists-p ()
  (should (exercism--cli-already-exists-p "directory already exists"))
  (should (not (exercism--cli-already-exists-p "Download complete"))))

(ert-deftest exercism--exercise-list-solved-p ()
  (should (exercism--exercise-list-solved-p "published"))
  (should (exercism--exercise-list-solved-p "completed"))
  (should (exercism--exercise-list-solved-p "Published"))
  (should (not (exercism--exercise-list-solved-p "started")))
  (should (not (exercism--exercise-list-solved-p nil))))

(ert-deftest exercism--exercise-list-state ()
  (let ((exercise '((slug . "hello-world") (is_unlocked . t))))
    (should (eq 'solved
                (exercism--exercise-list-state exercise "published")))
    (should (eq 'locked
                (exercism--exercise-list-state
                 '((slug . "secret") (is_unlocked . nil)) nil)))
    (should (eq 'not-started
                (exercism--exercise-list-state exercise nil)))
    (should (eq 'in-progress
                (exercism--exercise-list-state exercise "started")))))

(ert-deftest exercism--exercise-list-state-label ()
  (should (equal "solved"
                 (exercism-ert--label-text
                  (exercism--exercise-list-state-label 'solved))))
  (should (equal "in progress"
                 (exercism-ert--label-text
                  (exercism--exercise-list-state-label 'in-progress))))
  (should (equal "not started"
                 (exercism-ert--label-text
                  (exercism--exercise-list-state-label 'not-started))))
  (should (equal "locked"
                 (exercism-ert--label-text
                  (exercism--exercise-list-state-label 'locked)))))

(ert-deftest exercism--exercise-list-pending-label ()
  (should (equal "submitting "
                 (exercism-ert--label-text
                  (exercism--exercise-list-pending-label 'submitting 0))))
  (should (equal "submitting."
                 (exercism-ert--label-text
                  (exercism--exercise-list-pending-label 'submitting 1))))
  (should (equal "submitted"
                 (exercism-ert--label-text
                  (exercism--exercise-list-pending-label 'submitted))))
  (should (equal "failed"
                 (exercism-ert--label-text
                  (exercism--exercise-list-pending-label 'submit-failed)))))

(ert-deftest exercism-exercise-list--line-for-slug ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (should (exercism-exercise-list--line-for-slug "two-fer"))
     (should (null (exercism-exercise-list--line-for-slug "missing-slug"))))))

(ert-deftest exercism--submit-slug-updates-list-status ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (slug "reverse-string")
         (track-dir (expand-file-name track workspace))
         (exercise-dir (expand-file-name slug track-dir)))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir "reverse_string.go")
          (exercism-ert--with-exercise-list
           (list `((slug . ,slug)
                   (difficulty . "easy")
                   (blurb . "Reverse it")
                   (is_unlocked . t)))
           (exercism-ert--make-solution-table nil)
           (lambda ()
             (setq exercism--current-track track
                   exercism--workspace workspace
                   exercism-ert--submit-sync-result "Submitted successfully")
             (advice-add #'exercism--run-shell-command :around
                         #'exercism-ert--run-shell-command-immediate)
             (exercism--submit-slug slug)
             (let ((line (buffer-substring-no-properties
                          (car (exercism-exercise-list--line-for-slug slug))
                          (cdr (exercism-exercise-list--line-for-slug slug)))))
               (should (string-match-p "submitted" line))
               (should (eq 'submitted (gethash slug exercism--exercise-pending-states))))
             (advice-remove #'exercism--run-shell-command
                            #'exercism-ert--run-shell-command-immediate)))
      (exercism--submit-animation-stop)
      (clrhash exercism--exercise-pending-states)
      (when (file-exists-p workspace)
        (delete-directory workspace t))))))

(ert-deftest exercism--submit-slug-updates-list-status-on-error ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (slug "reverse-string")
         (track-dir (expand-file-name track workspace))
         (exercise-dir (expand-file-name slug track-dir)))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir "reverse_string.go")
          (exercism-ert--with-exercise-list
           (list `((slug . ,slug)
                   (difficulty . "easy")
                   (blurb . "Reverse it")
                   (is_unlocked . t)))
           (exercism-ert--make-solution-table nil)
           (lambda ()
             (setq exercism--current-track track
                   exercism--workspace workspace
                   exercism-ert--submit-sync-result "Error: submit failed")
             (advice-add #'exercism--run-shell-command :around
                         #'exercism-ert--run-shell-command-immediate)
             (exercism--submit-slug slug)
             (let ((line (buffer-substring-no-properties
                          (car (exercism-exercise-list--line-for-slug slug))
                          (cdr (exercism-exercise-list--line-for-slug slug)))))
               (should (string-match-p "failed" line))
               (should (eq 'submit-failed (gethash slug exercism--exercise-pending-states))))
             (advice-remove #'exercism--run-shell-command
                            #'exercism-ert--run-shell-command-immediate)))
      (exercism--submit-animation-stop)
      (clrhash exercism--exercise-pending-states)
      (when (file-exists-p workspace)
        (delete-directory workspace t))))))

(ert-deftest exercism--exercise-list-longest ()
  (should (zerop (exercism--exercise-list-longest nil 'slug)))
  (should (= 16 (exercism--exercise-list-longest
                 (exercism-ert--sample-exercises) 'slug))))

(ert-deftest exercism--semver-to-number ()
  (should (< (exercism--semver-to-number "3.2.0")
             (exercism--semver-to-number "3.26.1")))
  (should (= (exercism--semver-to-number "3.2.0")
             (exercism--semver-to-number "3.2.0"))))

(ert-deftest exercism--compare-semvers ()
  (should (exercism--compare-semvers "3.2.0" #'< "3.26.1"))
  (should (exercism--compare-semvers "3.26.1" #'>= "3.2.0"))
  (should (exercism--compare-semvers "3.2.0" #'= "3.2.0")))

(ert-deftest exercism--masked-token-short ()
  (should (equal "******" (exercism--masked-token "secret"))))

(ert-deftest exercism--masked-token-long ()
  (should (equal "abcd******************wxyz"
                 (exercism--masked-token "abcdefghijklmnopqrstuvwxyz"))))

(ert-deftest exercism--save-and-load-state ()
  (let ((state-file (make-temp-file "exercism-state" nil ".el")))
    (unwind-protect
        (cl-letf ((exercism--state-file state-file))
          (setq exercism--current-track "emacs-lisp"
                exercism--current-exercise "hello-world"
                exercism--workspace "/tmp/exercism-workspace")
          (exercism--save-state)
          (setq exercism--current-track nil
                exercism--current-exercise nil
                exercism--workspace nil)
          (exercism--load-state)
          (should (string= exercism--current-track "emacs-lisp"))
          (should (string= exercism--current-exercise "hello-world"))
          (should (string= exercism--workspace "/tmp/exercism-workspace")))
      (when (file-exists-p state-file)
        (delete-file state-file)))))

(ert-deftest exercism--reconcile-state-with-config-stale-workspace ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (state-file (make-temp-file "exercism-state" nil ".el"))
         (workspace (make-temp-file "exercism-workspace" 'dir))
         (go-dir (expand-file-name "go" workspace)))
    (unwind-protect
        (progn
          (make-directory go-dir t)
          (write-region
           (json-encode `((workspace . ,workspace) (token . "test-token")))
           nil config-file)
          (write-region
           (format "(setq exercism--current-track %S\n      exercism--current-exercise %S\n      exercism--workspace %S)\n"
                   "emacs-lisp" "hello-world" "/tmp/stale-workspace")
           nil state-file)
          (cl-letf ((exercism-config-path config-file)
                    (exercism--state-file state-file))
            (setq exercism--current-track nil
                  exercism--current-exercise nil
                  exercism--workspace "/tmp/stale-workspace")
            (exercism--load-state)
            (exercism--reconcile-state-with-config)
            (should (string= exercism--current-track "go"))
            (should (null exercism--current-exercise))
            (should (string= exercism--workspace workspace))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p state-file) (delete-file state-file))
      (when (file-exists-p workspace) (delete-directory workspace t)))))

(ert-deftest exercism--show-exercise-list-all ()
  (let ((exercises (exercism-ert--sample-exercises))
        (solutions (exercism-ert--make-solution-table
                     '(("hello-world" . "published")
                       ("two-fer" . "started")
                       ("bob" . nil)))))
    (unwind-protect
        (let ((exercism--current-track "emacs-lisp"))
          (exercism--show-exercise-list exercises solutions)
          (with-current-buffer exercism--exercise-list-buffer-name
            (let ((content (buffer-string)))
              (should (string-match-p "Exercism Exercises" content))
              (should (string-match-p "Track: emacs-lisp" content))
              (should (string-match-p "Exercises: 4" content))
              (should (string-match-p "Solved: 1 | Unsolved: 3" content))
              (should (string-match-p "hello-world" content))
              (should (string-match-p "two-fer" content))
              (should (string-match-p "secret-handshake" content)))))
      (when (get-buffer exercism--exercise-list-buffer-name)
        (kill-buffer exercism--exercise-list-buffer-name)))))

(ert-deftest exercism-exercise-list-orders-solved-last-stably ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("secret-handshake" . "completed")
      ("bob" . nil)))
   (lambda ()
     (should
      (equal '("two-fer" "bob" "hello-world" "secret-handshake")
             (exercism-ert--exercise-slugs-in-buffer))))))

(ert-deftest exercism-exercise-list-mode-activation ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("bob" . nil)))
   (lambda ()
     (should (derived-mode-p 'exercism-exercise-list-mode)))))

(ert-deftest exercism-exercise-list-row-properties ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("bob" . nil)))
   (lambda ()
     (exercism-ert--goto-exercise-slug "hello-world")
     (should (equal "hello-world"
                    (get-text-property (point) 'exercism-exercise-slug)))
     (should (get-text-property (point) 'exercism-exercise-unlocked))
     (exercism-ert--goto-exercise-slug "secret-handshake")
     (should (equal "secret-handshake"
                    (get-text-property (point) 'exercism-exercise-slug)))
     (should (not (get-text-property (point) 'exercism-exercise-unlocked))))))

(ert-deftest exercism-exercise-list-initial-point ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("bob" . nil)))
   (lambda ()
     (should (equal "two-fer"
                    (get-text-property (point) 'exercism-exercise-slug))))))

(ert-deftest exercism-exercise-list--slug-at-point ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("bob" . nil)))
   (lambda ()
     (exercism-ert--goto-exercise-slug "two-fer")
     (should (equal "two-fer" (exercism-exercise-list--slug-at-point)))
     (goto-char (point-min))
     (should (not (exercism-exercise-list--slug-at-point))))))

(ert-deftest exercism-exercise-list-next ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("bob" . nil)))
   (lambda ()
     (exercism-ert--goto-exercise-slug "two-fer")
     (exercism-exercise-list-next)
     (should (equal "secret-handshake"
                    (exercism-exercise-list--slug-at-point))))))

(ert-deftest exercism-exercise-list-previous ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("bob" . nil)))
   (lambda ()
     (exercism-ert--goto-exercise-slug "secret-handshake")
     (exercism-exercise-list-previous)
     (should (equal "two-fer" (exercism-exercise-list--slug-at-point))))))

(ert-deftest exercism-exercise-list-reload-key ()
  (should (eq #'exercism-exercise-list-reload
              (lookup-key exercism-exercise-list-mode-map "g"))))

(ert-deftest exercism-exercise-list-set-track-key ()
  (should (eq #'exercism-exercise-list-set-track
              (lookup-key exercism-exercise-list-mode-map "t"))))

(ert-deftest exercism--exercise-url ()
  (should (equal "https://exercism.org/tracks/emacs-lisp/exercises/two-fer"
                 (exercism--exercise-url "emacs-lisp" "two-fer"))))

(ert-deftest exercism-exercise-list-open-in-browser-key ()
  (should (eq #'exercism-exercise-list-open-in-browser
              (lookup-key exercism-exercise-list-mode-map "b"))))

(defvar exercism-ert--browse-url-target nil)

(defun exercism-ert--browse-url-recorder (url &rest _args)
  "Advice that records the URL passed to `browse-url'."
  (setq exercism-ert--browse-url-target url))

(ert-deftest exercism-exercise-list-open-in-browser ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (exercism-ert--goto-exercise-slug "two-fer")
     (setq exercism-ert--browse-url-target nil)
     (advice-add #'browse-url :override #'exercism-ert--browse-url-recorder)
     (unwind-protect
         (exercism-exercise-list-open-in-browser)
       (advice-remove #'browse-url #'exercism-ert--browse-url-recorder))
     (should (equal "https://exercism.org/tracks/emacs-lisp/exercises/two-fer"
                    exercism-ert--browse-url-target)))))

(ert-deftest exercism-exercise-list-open-in-browser-not-on-row ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (goto-char (point-min))
     (should-error (exercism-exercise-list-open-in-browser) :type 'user-error))))

(ert-deftest exercism-exercise-list-submit-key ()
  (should (eq #'exercism-exercise-list-submit-exercise
              (lookup-key exercism-exercise-list-mode-map "s"))))

(ert-deftest exercism-exercise-list-submit-exercise-locked ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (exercism-ert--goto-exercise-slug "secret-handshake")
     (should-error (exercism-exercise-list-submit-exercise) :type 'user-error))))

(ert-deftest exercism-exercise-list-submit-exercise-confirmed ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "two-fer")
         (exercise-dir (expand-file-name slug (expand-file-name track workspace))))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir "two_fer.el")
          (setq exercism--current-track track
                exercism--workspace workspace
                exercism-ert--submit-slug nil)
          (advice-add #'exercism--submit-slug :around #'exercism-ert--submit-slug-recorder)
          (exercism-ert--with-exercise-list
           (exercism-ert--sample-exercises)
           (exercism-ert--make-solution-table nil)
           (lambda ()
             (exercism-ert--goto-exercise-slug slug)
             (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
               (exercism-exercise-list-submit-exercise))
             (should (equal slug exercism-ert--submit-slug)))))
      (advice-remove #'exercism--submit-slug #'exercism-ert--submit-slug-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism-exercise-list-submit-exercise-declined ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (setq exercism-ert--submit-slug nil)
     (advice-add #'exercism--submit-slug :around #'exercism-ert--submit-slug-recorder)
     (unwind-protect
         (progn
           (exercism-ert--goto-exercise-slug "two-fer")
           (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
             (exercism-exercise-list-submit-exercise))
           (should (null exercism-ert--submit-slug)))
       (advice-remove #'exercism--submit-slug #'exercism-ert--submit-slug-recorder)))))

(ert-deftest exercism--submit-slug-not-downloaded ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "two-fer"))
    (unwind-protect
        (progn
          (setq exercism--current-track track
                exercism--workspace workspace)
          (should-error (exercism--submit-slug slug) :type 'user-error))
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism-exercise-list-buffer-settings ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (should (equal (exercism-ert--sample-exercises)
                    exercism-exercise-list-exercises)))))

(ert-deftest exercism-exercise-list-reload ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("bob" . nil)))
   (lambda ()
     (should (string-match-p "Solved: 1 | Unsolved: 3" (buffer-string)))
     (unwind-protect
         (progn
           (advice-add #'exercism--with-track-exercises-and-solutions :around
                       #'exercism-ert--with-track-recorder-all-solved)
           (exercism-exercise-list-reload)
           (should (string-match-p "Exercises: 4" (buffer-string)))
           (should (string-match-p "Solved: 4 | Unsolved: 0" (buffer-string))))
       (advice-remove #'exercism--with-track-exercises-and-solutions
                      #'exercism-ert--with-track-recorder-all-solved)))))

(ert-deftest exercism-exercise-list-unsolved-toggle-key-removed ()
  (should-not (lookup-key exercism-exercise-list-mode-map "u")))

(ert-deftest exercism--primary-solution-file ()
  (let* ((exercise-dir (make-temp-file "exercism-exercise" 'dir))
         (solution-file (expand-file-name "hello.el" exercise-dir)))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir "hello.el")
          (should (string= solution-file
                           (exercism--primary-solution-file exercise-dir))))
      (when (file-exists-p exercise-dir)
        (delete-directory exercise-dir t)))))

(ert-deftest exercism--solution-file-paths ()
  (let* ((exercise-dir (make-temp-file "exercism-exercise" 'dir))
         (solution-file (expand-file-name "hello.el" exercise-dir)))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir "hello.el")
          (should (equal (list solution-file)
                         (exercism--solution-file-paths exercise-dir))))
      (when (file-exists-p exercise-dir)
        (delete-directory exercise-dir t)))))

(ert-deftest exercism--submit-slug-uses-absolute-paths ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (slug "reverse-string")
         (track-dir (expand-file-name track workspace))
         (exercise-dir (expand-file-name slug track-dir))
         (solution-file (expand-file-name "reverse_string.go" exercise-dir)))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir "reverse_string.go")
          (setq exercism--current-track track
                exercism--workspace workspace
                exercism-ert--submit-command nil)
          (advice-add #'exercism--run-shell-command :around
                      #'exercism-ert--run-shell-command-recorder)
          (exercism--submit-slug slug)
          (should (string-match-p (regexp-quote solution-file)
                                  exercism-ert--submit-command))
          (should (not (string-match-p " submit reverse_string.go" exercism-ert--submit-command))))
      (advice-remove #'exercism--run-shell-command #'exercism-ert--run-shell-command-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism--open-exercise-slug-existing-dir ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "hello-world")
         (track-dir (expand-file-name track workspace))
         (exercise-dir (expand-file-name slug track-dir))
         (solution-file (expand-file-name "hello.el" exercise-dir)))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir "hello.el")
          (setq exercism--current-track track
                exercism--current-exercise nil
                exercism--workspace workspace
                exercism-ert--find-file-target nil)
          (advice-add #'find-file :around #'exercism-ert--find-file-recorder)
          (exercism--open-exercise-slug slug)
          (should (string= exercism--current-exercise slug))
          (should (string= exercism-ert--find-file-target solution-file)))
      (advice-remove #'find-file #'exercism-ert--find-file-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism--open-exercise-slug-downloads-missing ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "two-fer")
         (track-dir (expand-file-name track workspace)))
    (unwind-protect
        (progn
          (make-directory track-dir t)
          (setq exercism--current-track track
                exercism--current-exercise nil
                exercism--workspace workspace
                exercism-ert--download-args nil)
          (advice-add #'exercism--download-exercise :around
                      #'exercism-ert--download-exercise-recorder)
          (exercism--open-exercise-slug slug)
          (should (equal (list slug track) exercism-ert--download-args))
          (should (string= exercism--current-exercise slug)))
      (advice-remove #'exercism--download-exercise
                     #'exercism-ert--download-exercise-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism-exercise-list-open-exercise-locked ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (exercism-ert--goto-exercise-slug "secret-handshake")
     (should-error (exercism-exercise-list-open-exercise) :type 'user-error))))

(ert-deftest exercism-exercise-list-open-exercise-unlocked ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "two-fer"))
    (unwind-protect
        (progn
          (setq exercism--current-track track
                exercism--workspace workspace
                exercism-ert--opened-slug nil)
          (advice-add #'exercism--open-exercise-slug :around
                      #'exercism-ert--open-slug-recorder)
          (exercism-ert--with-exercise-list
           (exercism-ert--sample-exercises)
           (exercism-ert--make-solution-table
            '(("hello-world" . "published")
              ("two-fer" . "started")
              ("bob" . nil)))
           (lambda ()
             (exercism-ert--goto-exercise-slug slug)
             (exercism-exercise-list-open-exercise)
             (should (equal slug exercism-ert--opened-slug)))))
      (advice-remove #'exercism--open-exercise-slug
                     #'exercism-ert--open-slug-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(provide 'exercism-ert)
;;; exercism-ert.el ends here
