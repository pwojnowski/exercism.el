;;; exercism-tests.el --- ERT tests for exercism.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for pure helpers in `exercism.el'.
;; Run via `eldev test' or M-x ert after loading this file.

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
  (let ((exercism--track-icon-cache-root
         (make-temp-file "exercism-icon-cache" 'dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'exercism--ensure-current-track-icon)
                   (lambda (&rest _) nil)))
          (let ((exercism--current-track "emacs-lisp"))
            (exercism--show-exercise-list exercises solutions)
            (with-current-buffer exercism--exercise-list-buffer-name
              (funcall body))))
      (when (get-buffer exercism--exercise-list-buffer-name)
        (kill-buffer exercism--exercise-list-buffer-name))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

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

(defun exercism-ert--download-exercise-recorder (orig exercise-slug track-slug callback &optional force)
  "Advice that records download args and simulates success."
  (setq exercism-ert--download-args (list exercise-slug track-slug force))
  (let ((exercise-dir (expand-file-name exercise-slug
                                        (expand-file-name track-slug exercism--workspace))))
    (exercism-ert--write-minimal-exercise exercise-dir)
    (funcall callback 0 "Downloaded")))

(defvar exercism-ert--download-args nil)

(defun exercism-ert--open-slug-recorder (orig slug)
  "Advice that records the slug passed to `exercism--open-exercise-slug'."
  (setq exercism-ert--opened-slug slug))

(defvar exercism-ert--opened-slug nil)

(defun exercism-ert--download-slug-recorder (orig slug)
  "Advice that records the slug passed to `exercism--download-exercise-slug'."
  (setq exercism-ert--downloaded-slug slug))

(defvar exercism-ert--downloaded-slug nil)

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

(ert-deftest exercism--cli-rate-limited-p ()
  (should (exercism--cli-rate-limited-p "Error: 429 Too Many Requests"))
  (should (exercism--cli-rate-limited-p "rate limit exceeded"))
  (should (exercism--cli-rate-limited-p "Error: too many requests"))
  (should-not (exercism--cli-rate-limited-p "Downloaded exercise"))
  (should-not (exercism--cli-rate-limited-p "Error: token invalid")))

(ert-deftest exercism--download-succeeded-p ()
  (let ((exercise-dir (make-temp-file "exercism-dl-ok" 'dir)))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir)
          (should (exercism--download-succeeded-p 0 "Downloaded" exercise-dir))
          (should-not (exercism--download-succeeded-p 1 "Downloaded" exercise-dir))
          (should-not (exercism--download-succeeded-p 0 "Error: boom" exercise-dir)))
      (when (file-exists-p exercise-dir)
        (delete-directory exercise-dir t))))
  (let ((stub (make-temp-file "exercism-dl-stub" 'dir)))
    (unwind-protect
        (should-not (exercism--download-succeeded-p 0 "Downloaded" stub))
      (when (file-exists-p stub)
        (delete-directory stub t)))))

(ert-deftest exercism--download-exercise-force-flag ()
  (let ((exercism-executable "exercism")
        (captured nil))
    (cl-letf (((symbol-function 'exercism--run-shell-command-with-status)
               (lambda (shell-cmd &optional callback)
                 (setq captured shell-cmd)
                 (when callback (funcall callback 0 "ok")))))
      (exercism--download-exercise "two-fer" "go" #'ignore t)
      (should (string-match-p "--force" captured))
      (exercism--download-exercise "two-fer" "go" #'ignore)
      (should-not (string-match-p "--force" captured)))))

(ert-deftest exercism--exercise-downloaded-p-missing-dir ()
  (should-not (exercism--exercise-downloaded-p
               (expand-file-name "no-such-exercise"
                                 (make-temp-file "exercism-missing" 'dir)))))

(ert-deftest exercism--exercise-downloaded-p-metadata-only ()
  (let ((exercise-dir (make-temp-file "exercism-stub" 'dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".exercism" exercise-dir) t)
          (write-region "{\"track\":\"go\",\"exercise\":\"stub\"}"
                        nil
                        (expand-file-name ".exercism/metadata.json" exercise-dir))
          (should-not (exercism--exercise-downloaded-p exercise-dir)))
      (when (file-exists-p exercise-dir)
        (delete-directory exercise-dir t)))))

(ert-deftest exercism--exercise-downloaded-p-config-without-solution ()
  (let ((exercise-dir (make-temp-file "exercism-incomplete" 'dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".exercism" exercise-dir) t)
          (write-region "{\"files\":{\"solution\":[\"missing.el\"]}}"
                        nil
                        (expand-file-name ".exercism/config.json" exercise-dir))
          (should-not (exercism--exercise-downloaded-p exercise-dir)))
      (when (file-exists-p exercise-dir)
        (delete-directory exercise-dir t)))))

(ert-deftest exercism--exercise-downloaded-p-complete ()
  (let ((exercise-dir (make-temp-file "exercism-complete" 'dir)))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir "hello.el")
          (should (exercism--exercise-downloaded-p exercise-dir)))
      (when (file-exists-p exercise-dir)
        (delete-directory exercise-dir t)))))

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

(ert-deftest exercism--workspace-from-state-missing ()
  (let ((state-file (make-temp-file "exercism-state" nil ".el")))
    (unwind-protect
        (cl-letf ((exercism--state-file state-file))
          (delete-file state-file)
          (should (null (exercism--workspace-from-state))))
      (when (file-exists-p state-file)
        (delete-file state-file)))))

(ert-deftest exercism--workspace-from-state-present ()
  (let ((state-file (make-temp-file "exercism-state" nil ".el"))
        (workspace "/tmp/exercism-saved-workspace"))
    (unwind-protect
        (cl-letf ((exercism--state-file state-file))
          (setq exercism--current-track "go"
                exercism--current-exercise "hello-world"
                exercism--workspace workspace)
          (exercism--save-state)
          (setq exercism--current-track "emacs-lisp"
                exercism--current-exercise "two-fer"
                exercism--workspace "/tmp/other-workspace")
          (should (string= (exercism--workspace-from-state) workspace))
          (should (string= exercism--current-track "emacs-lisp"))
          (should (string= exercism--current-exercise "two-fer"))
          (should (string= exercism--workspace "/tmp/other-workspace")))
      (when (file-exists-p state-file)
        (delete-file state-file)))))

(ert-deftest exercism--workspace-configure-default-prefers-state ()
  (let ((state-file (make-temp-file "exercism-state" nil ".el"))
        (workspace "/tmp/exercism-configure-default"))
    (unwind-protect
        (cl-letf ((exercism--state-file state-file))
          (setq exercism--workspace workspace)
          (exercism--save-state)
          (should (string= (exercism--workspace-configure-default) workspace)))
      (when (file-exists-p state-file)
        (delete-file state-file)))))

(ert-deftest exercism--workspace-configure-default-fallback ()
  (let ((state-file (make-temp-file "exercism-state" nil ".el")))
    (unwind-protect
        (cl-letf ((exercism--state-file state-file))
          (delete-file state-file)
          (should (string= (exercism--workspace-configure-default)
                           exercism--default-workspace)))
      (when (file-exists-p state-file)
        (delete-file state-file)))))

(ert-deftest exercism--configure-includes-workspace ()
  (let ((config-file (make-temp-file "exercism-user" nil ".json"))
        (workspace (make-temp-file "exercism-workspace" 'dir)))
    (unwind-protect
        (progn
          (write-region "{}" nil config-file)
          (cl-letf ((exercism-config-path config-file))
            (advice-add #'exercism--run-shell-command :around
                        #'exercism-ert--run-shell-command-recorder)
            (exercism--configure "test-token" workspace)
            (should (string-match-p "--workspace" exercism-ert--submit-command))
            (should (string-match-p (regexp-quote workspace)
                                    exercism-ert--submit-command))
            (should (string-match-p "--token" exercism-ert--submit-command))))
      (advice-remove #'exercism--run-shell-command
                     #'exercism-ert--run-shell-command-recorder)
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t)))))

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

(ert-deftest exercism-exercise-list-summary-includes-track-icon ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table
    '(("hello-world" . "published")
      ("two-fer" . "started")
      ("bob" . nil)))
   (lambda ()
     (let ((icon (substring-no-properties
                  (exercism--track-icon-display "emacs-lisp")))
           (line (progn
                   (goto-char (point-min))
                   (search-forward "Track: emacs-lisp")
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))))
       (should (string-prefix-p icon line))
       (should (string-match-p "Track: emacs-lisp\\'" line))))))

(ert-deftest exercism--ensure-current-track-icon-skips-when-cached ()
  (let* ((exercism--track-icon-cache-root
          (make-temp-file "exercism-icon-cache" 'dir))
         (exercism--current-track "go")
         (path (exercism--track-icon-cache-path "go"))
         (fetch-called nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "<svg></svg>"))
          (cl-letf (((symbol-function 'exercism--fetch-track-icon)
                     (lambda (&rest _)
                       (setq fetch-called t))))
            (exercism--ensure-current-track-icon)
            (should (not fetch-called))))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism--ensure-current-track-icon-fetches-and-rerenders ()
  (let* ((exercism--track-icon-cache-root
          (make-temp-file "exercism-icon-cache" 'dir))
         (exercism--current-track "go")
         (fetched nil)
         (render-count 0))
    (unwind-protect
        (progn
          (with-current-buffer
              (get-buffer-create exercism--exercise-list-buffer-name)
            (exercism-exercise-list-mode))
          (cl-letf (((symbol-function 'exercism--fetch-track-icon)
                     (lambda (slug icon-url callback)
                       (setq fetched (list slug icon-url))
                       (funcall callback "/tmp/go.svg")))
                    ((symbol-function 'exercism--render-exercise-list)
                     (lambda ()
                       (setq render-count (1+ render-count)))))
            (exercism--ensure-current-track-icon)
            (should (equal fetched
                           '("go" "https://assets.exercism.org/tracks/go.svg")))
            (should (= 1 render-count))))
      (when (get-buffer exercism--exercise-list-buffer-name)
        (kill-buffer exercism--exercise-list-buffer-name))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism--ensure-current-track-icon-ignores-failed-fetch ()
  (let ((exercism--track-icon-cache-root
         (make-temp-file "exercism-icon-cache" 'dir))
        (exercism--current-track "go")
        (render-count 0))
    (unwind-protect
        (progn
          (with-current-buffer
              (get-buffer-create exercism--exercise-list-buffer-name)
            (exercism-exercise-list-mode))
          (cl-letf (((symbol-function 'exercism--fetch-track-icon)
                     (lambda (_slug _icon-url callback)
                       (funcall callback nil)))
                    ((symbol-function 'exercism--render-exercise-list)
                     (lambda ()
                       (setq render-count (1+ render-count)))))
            (exercism--ensure-current-track-icon)
            (should (= 0 render-count))))
      (when (get-buffer exercism--exercise-list-buffer-name)
        (kill-buffer exercism--exercise-list-buffer-name))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

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

(ert-deftest exercism--exercise-list-apply-track-in-buffer ()
  (let ((origin-buffer (generate-new-buffer " *exercism-origin*"))
        recorded-buffer
        recorded-track)
    (unwind-protect
        (progn
          (with-current-buffer origin-buffer
            (exercism-exercise-list-mode))
          (cl-letf (((symbol-function 'exercism--exercise-list-apply-track)
                     (lambda (track)
                       (setq recorded-buffer (current-buffer)
                             recorded-track track))))
            (with-temp-buffer
              (exercism--exercise-list-apply-track-in-buffer
               "emacs-lisp" origin-buffer)))
          (should (eq origin-buffer recorded-buffer))
          (should (equal "emacs-lisp" recorded-track)))
      (when (buffer-live-p origin-buffer)
        (kill-buffer origin-buffer)))))

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

(ert-deftest exercism-exercise-list-key-help-short ()
  (should (equal "RET open | s submit | r test | b browser | d download | D download all | t track"
                 exercism-exercise-list-key-help)))

(ert-deftest exercism-exercise-list-shows-short-key-help ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (let ((text (buffer-string)))
       (should (string-match-p
                "RET open | s submit | r test | b browser | d download | D download all | t track"
                text))
       (should-not (string-match-p "submit\\+browser" text))
       (should-not (string-match-p "self-check" text))))))

(ert-deftest exercism-exercise-list-download-keys ()
  (should (eq #'exercism-exercise-list-download-exercise
              (lookup-key exercism-exercise-list-mode-map "d")))
  (should (eq #'exercism-download-all-unlocked-exercises
              (lookup-key exercism-exercise-list-mode-map "D"))))

(ert-deftest exercism-exercise-list-help-key ()
  (should (eq #'exercism-exercise-list-show-help
              (lookup-key exercism-exercise-list-mode-map "?"))))

(ert-deftest exercism-exercise-list-self-check-key ()
  (should (eq #'exercism-self-check
              (lookup-key exercism-exercise-list-mode-map "C"))))

(ert-deftest exercism-exercise-list-submit-then-open-key-removed ()
  (should-not (lookup-key exercism-exercise-list-mode-map "S")))

(ert-deftest exercism-exercise-list-show-help ()
  (unwind-protect
      (progn
        (exercism-exercise-list-show-help)
        (with-current-buffer "*Exercism Exercises Help*"
          (should (derived-mode-p 'special-mode))
          (let ((text (buffer-string)))
            (should (string-match-p "RET\\s-+Open exercise" text))
            (should (string-match-p "s\\s-+Submit" text))
            (should (string-match-p "d\\s-+Download current" text))
            (should (string-match-p "D\\s-+Download all unlocked" text))
            (should (string-match-p "C\\s-+Self-check" text))
            (should (string-match-p "c\\s-+Configure" text))
            (should (string-match-p "g\\s-+Reload" text))
            (should (string-match-p "q\\s-+Quit" text))
            (should (string-match-p "\\?\\s-+This help" text)))))
    (when (get-buffer "*Exercism Exercises Help*")
      (kill-buffer "*Exercism Exercises Help*"))))

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
          (should (equal (list slug track nil) exercism-ert--download-args))
          (should (string= exercism--current-exercise slug)))
      (advice-remove #'exercism--download-exercise
                     #'exercism-ert--download-exercise-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism--download-exercise-slug-downloads-missing ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "two-fer")
         (track-dir (expand-file-name track workspace)))
    (unwind-protect
        (progn
          (make-directory track-dir t)
          (setq exercism--current-track track
                exercism--workspace workspace
                exercism-ert--download-args nil)
          (advice-add #'exercism--download-exercise :around
                      #'exercism-ert--download-exercise-recorder)
          (exercism--download-exercise-slug slug)
          (should (equal (list slug track nil) exercism-ert--download-args)))
      (advice-remove #'exercism--download-exercise
                     #'exercism-ert--download-exercise-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism--download-exercise-slug-repairs-incomplete ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (slug "complex-numbers")
         (exercise-dir (expand-file-name slug (expand-file-name track workspace))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".exercism" exercise-dir) t)
          (write-region "{\"track\":\"go\",\"exercise\":\"complex-numbers\"}"
                        nil
                        (expand-file-name ".exercism/metadata.json" exercise-dir))
          (setq exercism--current-track track
                exercism--workspace workspace
                exercism-ert--download-args nil)
          (advice-add #'exercism--download-exercise :around
                      #'exercism-ert--download-exercise-recorder)
          (exercism--download-exercise-slug slug)
          (should (equal (list slug track t) exercism-ert--download-args)))
      (advice-remove #'exercism--download-exercise
                     #'exercism-ert--download-exercise-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism--download-exercise-slug-skips-complete ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "two-fer")
         (exercise-dir (expand-file-name slug (expand-file-name track workspace))))
    (unwind-protect
        (progn
          (exercism-ert--write-minimal-exercise exercise-dir)
          (setq exercism--current-track track
                exercism--workspace workspace
                exercism-ert--download-args 'not-called)
          (advice-add #'exercism--download-exercise :around
                      #'exercism-ert--download-exercise-recorder)
          (exercism--download-exercise-slug slug)
          (should (eq 'not-called exercism-ert--download-args)))
      (advice-remove #'exercism--download-exercise
                     #'exercism-ert--download-exercise-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism--open-exercise-slug-repairs-incomplete ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (slug "complex-numbers")
         (exercise-dir (expand-file-name slug (expand-file-name track workspace))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".exercism" exercise-dir) t)
          (write-region "{\"track\":\"go\",\"exercise\":\"complex-numbers\"}"
                        nil
                        (expand-file-name ".exercism/metadata.json" exercise-dir))
          (setq exercism--current-track track
                exercism--current-exercise nil
                exercism--workspace workspace
                exercism-ert--download-args nil)
          (advice-add #'exercism--download-exercise :around
                      #'exercism-ert--download-exercise-recorder)
          (exercism--open-exercise-slug slug)
          (should (equal (list slug track t) exercism-ert--download-args))
          (should (string= exercism--current-exercise slug)))
      (advice-remove #'exercism--download-exercise
                     #'exercism-ert--download-exercise-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism--open-exercise-slug-download-failure ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "two-fer")
         (track-dir (expand-file-name track workspace)))
    (unwind-protect
        (progn
          (make-directory track-dir t)
          (setq exercism--current-track track
                exercism--current-exercise nil
                exercism--workspace workspace)
          (cl-letf (((symbol-function 'exercism--download-exercise)
                     (lambda (_slug _track callback &optional _force)
                       (funcall callback 1 "Error: 429 Too Many Requests"))))
            (exercism--open-exercise-slug slug)
            (should-not exercism--current-exercise)))
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism--track-init-repairs-incomplete-hello-world ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (hello-dir (expand-file-name "hello-world"
                                      (expand-file-name track workspace)))
         (callback-result nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".exercism" hello-dir) t)
          (write-region "{\"track\":\"go\",\"exercise\":\"hello-world\"}"
                        nil
                        (expand-file-name ".exercism/metadata.json" hello-dir))
          (setq exercism--workspace workspace
                exercism-ert--download-args nil)
          (advice-add #'exercism--download-exercise :around
                      #'exercism-ert--download-exercise-recorder)
          (exercism--track-init
           track
           (lambda (result) (setq callback-result result)))
          (should (equal (list "hello-world" track t) exercism-ert--download-args))
          (should (string= "Downloaded" callback-result)))
      (advice-remove #'exercism--download-exercise
                     #'exercism-ert--download-exercise-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism-download-all-skips-complete-and-queues-incomplete ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (track-dir (expand-file-name track workspace))
         (calls nil)
         (exercism--download-all-delay 0)
         (exercism--download-all-rate-limit-backoff 0)
         (exercism--download-all-schedule-fn
          (lambda (_seconds _repeat fn) (funcall fn))))
    (unwind-protect
        (progn
          (make-directory track-dir t)
          (exercism-ert--write-minimal-exercise
           (expand-file-name "hello-world" track-dir))
          (make-directory
           (expand-file-name "bob/.exercism" track-dir) t)
          (write-region "{}" nil
                        (expand-file-name "bob/.exercism/metadata.json" track-dir))
          (setq exercism--current-track track
                exercism--workspace workspace)
          (cl-letf (((symbol-function 'exercism--list-exercises)
                     (lambda (_track _only-unlocked callback)
                       (funcall callback
                                '(((slug . "hello-world") (is_unlocked . t))
                                  ((slug . "bob") (is_unlocked . t))
                                  ((slug . "two-fer") (is_unlocked . t))))))
                    ((symbol-function 'exercism--download-exercise)
                     (lambda (slug _track callback &optional force)
                       (push (list slug force) calls)
                       (exercism-ert--write-minimal-exercise
                        (exercism--exercise-dir-for-slug slug))
                       (funcall callback 0 "Downloaded"))))
            (exercism-download-all-unlocked-exercises)
            (should (equal (nreverse calls)
                           '(("bob" t) ("two-fer" nil))))))
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism-download-all-is-sequential ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (started nil)
         (pending-callbacks nil)
         (exercism--download-all-delay 0)
         (exercism--download-all-rate-limit-backoff 0)
         (exercism--download-all-schedule-fn
          (lambda (_seconds _repeat fn) (funcall fn))))
    (cl-labels ((complete-oldest ()
                  (let* ((entry (car (last pending-callbacks)))
                         (slug (car entry))
                         (cb (cdr entry)))
                    (setq pending-callbacks (butlast pending-callbacks))
                    (exercism-ert--write-minimal-exercise
                     (exercism--exercise-dir-for-slug slug))
                    (funcall cb 0 "Downloaded"))))
      (unwind-protect
          (progn
            (make-directory (expand-file-name track workspace) t)
            (setq exercism--current-track track
                  exercism--workspace workspace)
            (cl-letf (((symbol-function 'exercism--list-exercises)
                       (lambda (_track _only-unlocked callback)
                         (funcall callback
                                  '(((slug . "a") (is_unlocked . t))
                                    ((slug . "b") (is_unlocked . t))
                                    ((slug . "c") (is_unlocked . t))))))
                      ((symbol-function 'exercism--download-exercise)
                       (lambda (slug _track callback &optional _force)
                         (push slug started)
                         (push (cons slug callback) pending-callbacks))))
              (exercism-download-all-unlocked-exercises)
              (should (equal (nreverse (copy-sequence started)) '("a")))
              (complete-oldest)
              (should (equal (nreverse (copy-sequence started)) '("a" "b")))
              (complete-oldest)
              (should (equal (nreverse (copy-sequence started)) '("a" "b" "c")))))
        (when (file-exists-p workspace)
          (delete-directory workspace t))))))

(ert-deftest exercism-download-all-retries-once-on-rate-limit ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "go")
         (attempts 0)
         (exercism--download-all-delay 0)
         (exercism--download-all-rate-limit-backoff 0)
         (exercism--download-all-schedule-fn
          (lambda (_seconds _repeat fn) (funcall fn))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name track workspace) t)
          (setq exercism--current-track track
                exercism--workspace workspace)
          (cl-letf (((symbol-function 'exercism--list-exercises)
                     (lambda (_track _only-unlocked callback)
                       (funcall callback
                                '(((slug . "bob") (is_unlocked . t))))))
                    ((symbol-function 'exercism--download-exercise)
                     (lambda (slug _track callback &optional _force)
                       (setq attempts (1+ attempts))
                       (if (< attempts 2)
                           (funcall callback 1 "Error: 429 Too Many Requests")
                         (exercism-ert--write-minimal-exercise
                          (exercism--exercise-dir-for-slug slug))
                         (funcall callback 0 "Downloaded")))))
            (exercism-download-all-unlocked-exercises)
            (should (= 2 attempts))
            (should (exercism--exercise-downloaded-p
                     (exercism--exercise-dir-for-slug "bob")))))
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(ert-deftest exercism-exercise-list-open-exercise-locked ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (exercism-ert--goto-exercise-slug "secret-handshake")
     (should-error (exercism-exercise-list-open-exercise) :type 'user-error))))

(ert-deftest exercism-exercise-list-download-exercise-locked ()
  (exercism-ert--with-exercise-list
   (exercism-ert--sample-exercises)
   (exercism-ert--make-solution-table nil)
   (lambda ()
     (exercism-ert--goto-exercise-slug "secret-handshake")
     (should-error (exercism-exercise-list-download-exercise) :type 'user-error))))

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

(ert-deftest exercism-exercise-list-download-exercise-unlocked ()
  (let* ((workspace (make-temp-file "exercism-workspace" 'dir))
         (track "emacs-lisp")
         (slug "two-fer"))
    (unwind-protect
        (progn
          (setq exercism--current-track track
                exercism--workspace workspace
                exercism-ert--downloaded-slug nil)
          (advice-add #'exercism--download-exercise-slug :around
                      #'exercism-ert--download-slug-recorder)
          (exercism-ert--with-exercise-list
           (exercism-ert--sample-exercises)
           (exercism-ert--make-solution-table
            '(("hello-world" . "published")
              ("two-fer" . "started")
              ("bob" . nil)))
           (lambda ()
             (exercism-ert--goto-exercise-slug slug)
             (exercism-exercise-list-download-exercise)
             (should (equal slug exercism-ert--downloaded-slug)))))
      (advice-remove #'exercism--download-exercise-slug
                     #'exercism-ert--download-slug-recorder)
      (when (file-exists-p workspace)
        (delete-directory workspace t)))))

(defun exercism-ert--sample-tracks ()
  "Return fixture track alists for tests."
  '(((slug . "emacs-lisp")
     (title . "Emacs Lisp")
     (course . nil)
     (num_concepts . 0)
     (num_exercises . 86)
     (num_learnt_concepts . 0)
     (num_completed_exercises . 5)
     (is_joined . t)
     (is_new . nil)
     (has_notifications . t)
     (last_touched_at . "2024-03-15T10:00:00Z")
     (web_url . "https://exercism.org/tracks/emacs-lisp")
     (icon_url . "https://assets.exercism.org/tracks/emacs-lisp.svg"))
    ((slug . "go")
     (title . "Go")
     (course . nil)
     (num_concepts . 25)
     (num_exercises . 140)
     (is_joined . nil)
     (is_new . t)
     (has_notifications . nil)
     (last_touched_at . :null)
     (web_url . "https://exercism.org/tracks/go")
     (icon_url . "https://assets.exercism.org/tracks/go.svg"))
    ((slug . "python")
     (title . "Python")
     (course . t)
     (num_concepts . 30)
     (num_exercises . 120)
     (num_learnt_concepts . 12)
     (num_completed_exercises . 40)
     (is_joined . t)
     (is_new . nil)
     (has_notifications . nil)
     (last_touched_at . "2025-01-02T08:30:00Z")
     (web_url . "https://exercism.org/tracks/python")
     (icon_url . "https://assets.exercism.org/tracks/python.svg"))))

(defun exercism-ert--with-track-list (tracks auth-present-p body)
  "Show track list in a temp buffer and run BODY there."
  (let ((exercism--track-icon-cache-root (make-temp-file "exercism-icon-cache" 'dir)))
    (unwind-protect
        (progn
          (exercism--show-track-list tracks (lambda (_slug) nil))
          (with-current-buffer exercism--track-list-buffer-name
            (setq exercism-track-list-auth-present-p auth-present-p)
            (exercism--render-track-list)
            (funcall body)))
      (when (get-buffer exercism--track-list-buffer-name)
        (kill-buffer exercism--track-list-buffer-name))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(defun exercism-ert--with-authenticated-track-list (tracks body)
  "Show an authenticated track list and run BODY there."
  (let ((exercism--api-token "test-token"))
    (exercism-ert--with-track-list tracks t body)))

(defun exercism-ert--track-slugs-in-buffer ()
  "Return track slugs in their displayed order."
  (let (slugs)
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((slug (get-text-property (point) 'exercism-track-slug)))
        (push slug slugs))
      (forward-line 1))
    (nreverse slugs)))

(defun exercism-ert--goto-track-slug (slug)
  "Move point to the track row for SLUG in the current buffer."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (equal slug (get-text-property (point) 'exercism-track-slug))
        (throw 'found t))
      (forward-line 1))
    (error "Track row not found: %s" slug)))

(ert-deftest exercism--json-bool ()
  (should (exercism--json-bool t))
  (should (not (exercism--json-bool nil)))
  (should (exercism--json-bool :json-true))
  (should (not (exercism--json-bool :json-false))))

(ert-deftest exercism--track-list-enrollment-label ()
  (should (equal "Joined"
                 (substring-no-properties
                  (exercism--track-list-enrollment-label t t))))
  (should (equal "Not joined"
                 (substring-no-properties
                  (exercism--track-list-enrollment-label nil t))))
  (should (equal "—"
                 (exercism--track-list-enrollment-label t nil))))

(ert-deftest exercism--track-list-pad-right ()
  (should (equal "  140" (exercism--track-list-pad-right "140" 5)))
  (should (equal "40/120" (exercism--track-list-pad-right "40/120" 6)))
  (should (equal " 12/30" (exercism--track-list-pad-right "12/30" 6))))

(ert-deftest exercism--track-list-show-progress-p ()
  (should (exercism--track-list-show-progress-p t t))
  (should-not (exercism--track-list-show-progress-p t nil))
  (should-not (exercism--track-list-show-progress-p nil t)))

(ert-deftest exercism--track-list-progress-label ()
  (should (equal "5/12" (exercism--track-list-progress-label 5 12 t)))
  (should (equal "0/12" (exercism--track-list-progress-label nil 12 t)))
  (should (equal "12" (exercism--track-list-progress-label 5 12 nil)))
  (should (equal "0" (exercism--track-list-progress-label nil nil nil))))

(ert-deftest exercism--track-list-concepts-label ()
  (should (fboundp 'exercism--track-list-concepts-label))
  (should (equal "—" (exercism--track-list-concepts-label 0 0 t)))
  (should (equal "5/12" (exercism--track-list-concepts-label 5 12 t)))
  (should (equal "0/12" (exercism--track-list-concepts-label nil 12 t)))
  (should (equal "12" (exercism--track-list-concepts-label 5 12 nil))))

(ert-deftest exercism--track-list-type-label ()
  (should (equal "course" (exercism--track-list-type-label t)))
  (should (equal "practice" (exercism--track-list-type-label nil))))

(ert-deftest exercism--track-list-is-new-label ()
  (should (equal "new"
                 (exercism-ert--label-text
                  (exercism--track-list-is-new-label t))))
  (should (equal "" (exercism--track-list-is-new-label nil))))

(ert-deftest exercism--track-list-notifications-label ()
  (should (equal "notify"
                 (exercism-ert--label-text
                  (exercism--track-list-notifications-label t t))))
  (should (equal "" (exercism--track-list-notifications-label nil t)))
  (should (equal "—" (exercism--track-list-notifications-label t nil))))

(ert-deftest exercism--track-list-last-touched-label ()
  (should (equal "2024-03-15"
                 (exercism--track-list-last-touched-label "2024-03-15T10:00:00Z")))
  (should (equal "—" (exercism--track-list-last-touched-label nil)))
  (should (equal "—" (exercism--track-list-last-touched-label :null))))

(ert-deftest exercism--asset-request-headers ()
  (should (equal exercism--http-user-agent
                 (cdr (assoc "User-Agent" (exercism--asset-request-headers))))))

(ert-deftest exercism--fetch-track-icon-uses-native-asynchronous-retrieval ()
  (let ((exercism--track-icon-cache-root
         (make-temp-file "exercism-icon-cache" 'dir))
        retrieval-arguments
        retrieval-headers)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (&rest arguments)
                     (setq retrieval-arguments arguments
                           retrieval-headers url-request-extra-headers))))
          (exercism--fetch-track-icon
           "go" "https://assets.exercism.org/tracks/go.svg"
           #'ignore)
          (should retrieval-arguments)
          (should (equal (exercism--asset-request-headers)
                         retrieval-headers))
          (should (eq t (nth 3 retrieval-arguments))))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism--track-icon-cache-path ()
  (let ((exercism--track-icon-cache-root (make-temp-file "exercism-icon-cache" 'dir)))
    (unwind-protect
        (should (string-match-p "/emacs-lisp\\.svg\\'"
                                (exercism--track-icon-cache-path "emacs-lisp")))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism--track-icon-fallback-display ()
  (should (equal " G"
                 (substring-no-properties
                  (exercism--track-icon-fallback-display "go")))))

(ert-deftest exercism--track-icon-separator-aligns-title-by-pixels ()
  (let ((exercism--track-icon-size 16))
    (cl-letf (((symbol-function 'frame-char-width)
               (lambda (&optional _frame) 8)))
      (let ((separator
             (and (fboundp 'exercism--track-icon-separator)
                  (exercism--track-icon-separator))))
        (should separator)
        (should (equal " " (substring-no-properties separator)))
        (should (equal '(space :align-to (24))
                       (get-text-property 0 'display separator)))))))

(ert-deftest exercism--track-icon-image-uses-create-image-data-api ()
  (let* ((exercism--track-icon-cache-root
          (make-temp-file "exercism-icon-cache" 'dir))
         (path (exercism--track-icon-cache-path "go"))
         create-image-arguments)
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "<svg></svg>"))
          (cl-letf (((symbol-function 'image-type-available-p)
                     (lambda (_type) t))
                    ((symbol-function 'create-image)
                     (lambda (&rest arguments)
                       (setq create-image-arguments arguments)
                       '(image :type svg))))
            (should (equal '(image :type svg)
                           (exercism--track-icon-image path)))
            (should (equal
                     '("<svg></svg>" svg t
                       :ascent center :height 16 :width 16)
                     create-image-arguments))))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism--prefetch-track-icons-renders-once-after-batch ()
  (let ((render-count 0))
    (with-temp-buffer
      (exercism-track-list-mode)
      (rename-buffer exercism--track-list-buffer-name t)
      (cl-letf (((symbol-function 'exercism--fetch-track-icon)
                 (lambda (_slug _icon-url callback)
                   (funcall callback "/tmp/icon.svg")))
                ((symbol-function 'exercism--render-track-list)
                 (lambda ()
                   (setq render-count (1+ render-count)))))
        (exercism--prefetch-track-icons (exercism-ert--sample-tracks))
        (should (= 1 render-count))))))

(ert-deftest exercism--track-icon-display-invalid-file ()
  (let* ((exercism--track-icon-cache-root (make-temp-file "exercism-icon-cache" 'dir))
         (path (exercism--track-icon-cache-path "broken")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "not an svg"))
          (should (equal " B"
                         (substring-no-properties
                          (exercism--track-icon-display "broken")))))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism--svg-file-p ()
  (let* ((exercism--track-icon-cache-root (make-temp-file "exercism-icon-cache" 'dir))
         (svg-path (exercism--track-icon-cache-path "valid"))
         (txt-path (expand-file-name "invalid.svg"
                                     (exercism--track-icon-cache-dir))))
    (unwind-protect
        (progn
          (with-temp-file svg-path
            (insert "<svg></svg>"))
          (with-temp-file txt-path
            (insert "nope"))
          (should (exercism--svg-file-p svg-path))
          (should (not (exercism--svg-file-p txt-path))))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism--show-track-list-all ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (should (derived-mode-p 'exercism-track-list-mode))
     (should (equal '("emacs-lisp" "go" "python")
                    (exercism-ert--track-slugs-in-buffer)))
     (goto-char (point-min))
     (should (search-forward "Enrollment" nil t))
     (should (search-forward "Joined" nil t))
     (should (search-forward "notify" nil t))
     (should (search-forward "new" nil t)))))

(ert-deftest exercism-track-list-renders-zero-concepts-as-unavailable ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (exercism-ert--goto-track-slug "emacs-lisp")
     (should-not
      (string-match-p
       "0/0"
       (buffer-substring-no-properties
        (line-beginning-position) (line-end-position)))))))

(ert-deftest exercism-track-list-mode-activation ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (should (derived-mode-p 'exercism-track-list-mode)))))

(ert-deftest exercism-track-list-row-properties ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (exercism-ert--goto-track-slug "go")
     (should (equal "go" (exercism-track-list--slug-at-point))))))

(ert-deftest exercism-track-list-right-aligns-count-columns ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (let ((fields
            (mapcar
             (lambda (slug)
               (exercism-ert--goto-track-slug slug)
               (let ((line (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
                 (and (string-match
                       "  \\([ \t]*[0-9/—][0-9/—]*\\)  \\(?:practice\\|course\\)"
                       line)
                      (match-string 1 line))))
             '("go" "python" "emacs-lisp"))))
       (should (not (member nil fields)))
       (let ((widths (mapcar #'length fields)))
         (should (= (length (seq-uniq widths)) 1))
         (should (string-match-p "\\`[ \t]+" (car fields))))))))

(ert-deftest exercism-track-list-shows-totals-for-unjoined-tracks ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (exercism-ert--goto-track-slug "go")
     (let ((line (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
       (should (string-match-p "Not joined" line))
       (should (string-match-p "25" line))
       (should (string-match-p "140" line))
       (should-not (string-match-p "0/140" line))
       (should-not (string-match-p "0/25" line))))))

(ert-deftest exercism-track-list-does-not-render-slug ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (exercism-ert--goto-track-slug "emacs-lisp")
     (should-not
      (string-match-p
       "  emacs-lisp\\'"
       (buffer-substring-no-properties
        (line-beginning-position) (line-end-position)))))))

(ert-deftest exercism-track-list--slug-at-point ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (exercism-ert--goto-track-slug "python")
     (should (equal "python" (exercism-track-list--slug-at-point)))
     (goto-char (point-min))
     (should (not (exercism-track-list--slug-at-point))))))

(ert-deftest exercism-track-list-next ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (exercism-ert--goto-track-slug "emacs-lisp")
     (exercism-track-list-next)
     (should (equal "go" (exercism-track-list--slug-at-point))))))

(ert-deftest exercism-track-list-previous ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) t
   (lambda ()
     (exercism-ert--goto-track-slug "go")
     (exercism-track-list-previous)
     (should (equal "emacs-lisp" (exercism-track-list--slug-at-point))))))

(ert-deftest exercism--show-track-list-displays-in-origin-window ()
  (let ((origin-buffer (generate-new-buffer " *exercism-track-origin*"))
        (exercism--track-icon-cache-root
         (make-temp-file "exercism-icon-cache" 'dir)))
    (unwind-protect
        (progn
          (switch-to-buffer origin-buffer)
          (exercism--show-track-list
           (exercism-ert--sample-tracks)
           (lambda (_slug) nil))
          (should
           (eq (get-buffer exercism--track-list-buffer-name)
               (window-buffer (selected-window)))))
      (when (get-buffer exercism--track-list-buffer-name)
        (kill-buffer exercism--track-list-buffer-name))
      (when (buffer-live-p origin-buffer)
        (kill-buffer origin-buffer))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism-track-list-select-track ()
  (let ((selected nil)
        (callback-buffer nil)
        (origin-buffer (generate-new-buffer " *exercism-track-test*"))
        (exercism--track-icon-cache-root (make-temp-file "exercism-icon-cache" 'dir))
        (exercism--api-token "test-token"))
    (unwind-protect
        (progn
          (with-current-buffer origin-buffer
            (exercism-exercise-list-mode))
          (switch-to-buffer origin-buffer)
          (exercism--show-track-list
           (exercism-ert--sample-tracks)
           (lambda (slug)
             (setq selected slug
                   callback-buffer (current-buffer))))
          (with-current-buffer exercism--track-list-buffer-name
            (setq exercism-track-list-auth-present-p t)
            (exercism-ert--goto-track-slug "python")
            (exercism-track-list-select-track))
          (should (equal "python" selected))
          (should (eq origin-buffer callback-buffer))
          (should (eq origin-buffer (window-buffer (selected-window))))
          (should (not (get-buffer exercism--track-list-buffer-name))))
      (when (get-buffer exercism--track-list-buffer-name)
        (kill-buffer exercism--track-list-buffer-name))
      (when (buffer-live-p origin-buffer)
        (kill-buffer origin-buffer))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism-track-list--track-at-point ()
  (exercism-ert--with-authenticated-track-list (exercism-ert--sample-tracks)
   (lambda ()
     (exercism-ert--goto-track-slug "go")
     (let ((track (exercism-track-list--track-at-point)))
       (should track)
       (should (equal "go"
                      (exercism--json-value (exercism--plist-get track 'slug))))
       (should-not (exercism--track-joined-p track))))))

(ert-deftest exercism--track-web-url ()
  (let ((track (car (exercism-ert--sample-tracks))))
    (should (equal "https://exercism.org/tracks/emacs-lisp"
                   (exercism--track-web-url track))))
  (should (equal "https://exercism.org/tracks/rust"
                 (exercism--track-web-url '((slug . "rust"))))))

(ert-deftest exercism-track-list-select-track-requires-auth ()
  (exercism-ert--with-track-list (exercism-ert--sample-tracks) nil
   (lambda ()
     (exercism-ert--goto-track-slug "python")
     (should-error
      (exercism-track-list-select-track)
      :type 'user-error))))

(ert-deftest exercism-track-list-select-track-not-on-row ()
  (exercism-ert--with-authenticated-track-list (exercism-ert--sample-tracks)
   (lambda ()
     (goto-char (point-min))
     (should-error
      (exercism-track-list-select-track)
      :type 'user-error))))

(ert-deftest exercism-track-list-select-track-unjoined-opens-browser ()
  (let (opened-url)
    (exercism-ert--with-authenticated-track-list (exercism-ert--sample-tracks)
     (lambda ()
       (cl-letf (((symbol-function 'browse-url)
                  (lambda (url) (setq opened-url url)))
                 ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
         (exercism-ert--goto-track-slug "go")
         (exercism-track-list-select-track)
         (should (equal "https://exercism.org/tracks/go" opened-url))
         (should (get-buffer exercism--track-list-buffer-name)))))))

(ert-deftest exercism-track-list-select-track-unjoined-confirmed-verified ()
  (let ((selected nil))
    (exercism-ert--with-authenticated-track-list (exercism-ert--sample-tracks)
     (lambda ()
       (setq exercism-track-list-on-select (lambda (slug) (setq selected slug)))
       (cl-letf (((symbol-function 'browse-url) #'ignore)
                 ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                 ((symbol-function 'exercism--list-tracks)
                  (lambda (callback &optional _error-callback)
                    (funcall callback
                             (mapcar
                              (lambda (track)
                                (if (equal "go"
                                           (exercism--json-value
                                            (exercism--plist-get track 'slug)))
                                    (append (list (cons 'is_joined t))
                                            (cl-remove-if
                                             (lambda (pair)
                                               (eq (car pair) 'is_joined))
                                             track))
                                  track))
                              (exercism-ert--sample-tracks))))))
         (exercism-ert--goto-track-slug "go")
         (exercism-track-list-select-track)
         (should (equal "go" selected))
         (should (not (get-buffer exercism--track-list-buffer-name))))))))

(ert-deftest exercism-track-list-select-track-unjoined-declined ()
  (let ((selected nil))
    (exercism-ert--with-authenticated-track-list (exercism-ert--sample-tracks)
     (lambda ()
       (setq exercism-track-list-on-select (lambda (slug) (setq selected slug)))
       (cl-letf (((symbol-function 'browse-url) #'ignore)
                 ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
         (exercism-ert--goto-track-slug "go")
         (exercism-track-list-select-track)
         (should (null selected))
         (should (get-buffer exercism--track-list-buffer-name)))))))

(ert-deftest exercism-track-list-select-track-unjoined-not-verified ()
  (let ((selected nil)
        (messages nil))
    (exercism-ert--with-authenticated-track-list (exercism-ert--sample-tracks)
     (lambda ()
       (setq exercism-track-list-on-select (lambda (slug) (setq selected slug)))
       (cl-letf (((symbol-function 'browse-url) #'ignore)
                 ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                 ((symbol-function 'exercism--list-tracks)
                  (lambda (callback &optional _error-callback)
                    (funcall callback (exercism-ert--sample-tracks))))
                 ((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (push (apply #'format format-string args) messages))))
         (exercism-ert--goto-track-slug "go")
         (exercism-track-list-select-track)
         (should (null selected))
         (should (get-buffer exercism--track-list-buffer-name))
         (should (member "[exercism] enrollment was not detected for Go"
                         messages)))))))

(ert-deftest exercism-track-list-select-track-refresh-failure ()
  (exercism-ert--with-authenticated-track-list (exercism-ert--sample-tracks)
   (lambda ()
     (cl-letf (((symbol-function 'browse-url) #'ignore)
               ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
               ((symbol-function 'exercism--list-tracks)
                (lambda (_callback &optional error-callback)
                  (funcall error-callback
                           :error-thrown "network down"
                           :response nil))))
       (exercism-ert--goto-track-slug "go")
       (should-error
        (exercism-track-list-select-track)
        :type 'user-error)))))

(ert-deftest exercism-track-list-select-track-buffer-lifecycle ()
  (let ((selected nil))
    (exercism-ert--with-authenticated-track-list (exercism-ert--sample-tracks)
     (lambda ()
       (setq exercism-track-list-on-select
             (lambda (slug)
               (setq selected slug)
               (kill-buffer exercism--track-list-buffer-name)))
       (exercism-ert--goto-track-slug "python")
       (exercism-track-list-select-track)
       (should (equal "python" selected))
       (should (not (get-buffer exercism--track-list-buffer-name)))))))

(ert-deftest exercism-track-list-cancel-restores-origin ()
  (let ((origin-buffer (generate-new-buffer " *exercism-cancel-test*"))
        (exercism--track-icon-cache-root (make-temp-file "exercism-icon-cache" 'dir)))
    (unwind-protect
        (progn
          (with-current-buffer origin-buffer
            (exercism-exercise-list-mode))
          (switch-to-buffer origin-buffer)
          (exercism--show-track-list
           (exercism-ert--sample-tracks)
           (lambda (_slug) nil))
          (should (get-buffer exercism--track-list-buffer-name))
          (with-current-buffer exercism--track-list-buffer-name
            (exercism-track-list-cancel))
          (should (not (get-buffer exercism--track-list-buffer-name)))
          (should (eq origin-buffer (window-buffer (selected-window)))))
      (when (get-buffer exercism--track-list-buffer-name)
        (kill-buffer exercism--track-list-buffer-name))
      (when (buffer-live-p origin-buffer)
        (kill-buffer origin-buffer))
      (when (file-exists-p exercism--track-icon-cache-root)
        (delete-directory exercism--track-icon-cache-root t)))))

(ert-deftest exercism-track-list-reload-key ()
  (should (eq #'exercism-track-list-reload
              (lookup-key exercism-track-list-mode-map "g"))))

(ert-deftest exercism-track-list-select-key ()
  (should (eq #'exercism-track-list-select-track
              (lookup-key exercism-track-list-mode-map (kbd "RET")))))

(ert-deftest exercism-track-list-quit-key ()
  (should (eq #'exercism-track-list-cancel
              (lookup-key exercism-track-list-mode-map "q"))))

(defun exercism-ert--write-user-config (config-file workspace &optional token)
  "Write Exercism user config to CONFIG-FILE."
  (write-region
   (json-encode `((workspace . ,workspace)
                  (token . ,(or token "test-token"))))
   nil config-file))

(defun exercism-ert--make-fake-cli ()
  "Return a temporary executable used as the Exercism CLI in tests."
  (let ((path (make-temp-file "exercism-cli" nil ".sh")))
    (write-region "#!/bin/sh\n" nil path)
    (chmod path #o755)
    path))

(defun exercism-ert--make-fake-cli-with-version (version)
  "Return a temporary Exercism CLI executable reporting VERSION."
  (let ((path (make-temp-file "exercism-cli" nil ".sh")))
    (write-region
     (format "#!/bin/sh\nif [ \"$1\" = \"version\" ]; then echo \"exercism version %s\"; fi\n"
             version)
     nil path)
    (chmod path #o755)
    path))

(defun exercism-ert--with-valid-setup (config-file workspace body)
  "Run BODY with a valid local Exercism setup using CONFIG-FILE and WORKSPACE."
  (exercism-ert--write-user-config config-file workspace)
  (let ((exercism-executable (exercism-ert--make-fake-cli)))
    (unwind-protect
        (let ((exercism-config-path config-file)
              (exercism--workspace workspace))
          (when (boundp 'exercism--api-token)
            (makunbound 'exercism--api-token))
          (funcall body))
      (when (file-exists-p exercism-executable)
        (delete-file exercism-executable)))))

(defvar exercism-ert--self-check-called nil)

(defun exercism-ert--self-check-recorder (&rest _args)
  "Advice that records calls to `exercism-self-check'."
  (setq exercism-ert--self-check-called t))

(defvar exercism-ert--prompt-for-track-called nil)

(defun exercism-ert--prompt-for-track-recorder (_on-select)
  "Advice that records calls to `exercism--prompt-for-track'."
  (setq exercism-ert--prompt-for-track-called t))

(ert-deftest exercism--setup-ok-p-valid-config ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir)))
    (unwind-protect
        (exercism-ert--with-valid-setup config-file workspace
         (lambda ()
           (should (exercism--setup-ok-p))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t)))))

(ert-deftest exercism--setup-ok-p-missing-token ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir))
         (cli (exercism-ert--make-fake-cli)))
    (unwind-protect
        (progn
          (exercism-ert--write-user-config config-file workspace "")
          (let ((exercism-config-path config-file)
                (exercism-executable cli)
                (exercism--workspace workspace))
            (when (boundp 'exercism--api-token)
              (makunbound 'exercism--api-token))
            (should-not (exercism--setup-ok-p))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (file-exists-p cli) (delete-file cli)))))

(ert-deftest exercism--setup-ok-p-missing-config ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir))
         (cli (exercism-ert--make-fake-cli)))
    (unwind-protect
        (progn
          (delete-file config-file)
          (let ((exercism-config-path config-file)
                (exercism-executable cli)
                (exercism--workspace workspace))
            (when (boundp 'exercism--api-token)
              (makunbound 'exercism--api-token))
            (should-not (exercism--setup-ok-p))))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (file-exists-p cli) (delete-file cli)))))

(ert-deftest exercism-starts-self-check-when-unconfigured ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (cli (exercism-ert--make-fake-cli)))
    (unwind-protect
        (cl-letf (((symbol-function 'exercism-self-check)
                   #'exercism-ert--self-check-recorder))
          (write-region "{}" nil config-file)
          (setq exercism-ert--self-check-called nil
                exercism--current-track nil)
          (when (boundp 'exercism--api-token)
            (makunbound 'exercism--api-token))
          (let ((exercism-config-path config-file)
                (exercism-executable cli))
            (exercism)
            (should exercism-ert--self-check-called)
            (should-not (get-buffer exercism--exercise-list-buffer-name))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p cli) (delete-file cli))
      (when (get-buffer exercism--exercise-list-buffer-name)
        (kill-buffer exercism--exercise-list-buffer-name)))))

(ert-deftest exercism-starts-track-picker-when-no-track ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'exercism--prompt-for-track)
                   #'exercism-ert--prompt-for-track-recorder))
          (setq exercism-ert--prompt-for-track-called nil
                exercism--current-track nil)
          (exercism-ert--with-valid-setup config-file workspace
           (lambda ()
             (exercism)
             (should exercism-ert--prompt-for-track-called)
             (should-not (get-buffer exercism--exercise-list-buffer-name)))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (get-buffer exercism--exercise-list-buffer-name)
        (kill-buffer exercism--exercise-list-buffer-name)))))

(defun exercism-ert--run-self-check-without-async ()
  "Run `exercism-self-check' without network or background threads."
  (cl-letf (((symbol-function 'exercism--self-check-async) #'ignore)
            ((symbol-function 'request) #'ignore))
    (exercism-self-check)))

(ert-deftest exercism-self-check-hides-track-when-setup-fails ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir))
         (cli (exercism-ert--make-fake-cli)))
    (unwind-protect
        (progn
          (delete-file config-file)
          (let ((exercism-config-path config-file)
                (exercism-executable cli)
                (exercism--workspace workspace)
                (exercism--current-track "go")
                (exercism--current-exercise "hello-world"))
            (when (boundp 'exercism--api-token)
              (makunbound 'exercism--api-token))
            (exercism-ert--run-self-check-without-async)
            (with-current-buffer "*exercism-self-check*"
              (should-not (string-match-p "State file" (buffer-string)))
              (should-not (string-match-p "Current track" (buffer-string)))
              (should-not (string-match-p "Current exercise" (buffer-string))))))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (file-exists-p cli) (delete-file cli))
      (when (get-buffer "*exercism-self-check*")
        (kill-buffer "*exercism-self-check*")))))

(ert-deftest exercism-self-check-shows-track-when-setup-ok ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir)))
    (unwind-protect
        (exercism-ert--with-valid-setup config-file workspace
         (lambda ()
           (setq exercism--current-track "go"
                 exercism--current-exercise "hello-world")
           (exercism-ert--run-self-check-without-async)
           (with-current-buffer "*exercism-self-check*"
             (let ((content (buffer-string)))
               (should (string-match-p (regexp-quote exercism--state-file) content))
               (should (string-match-p "Current track: go" content))
               (should (string-match-p "Current exercise: hello-world" content))
               (should (< (string-match "State file" content)
                          (string-match "Current track" content)))))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (get-buffer "*exercism-self-check*")
        (kill-buffer "*exercism-self-check*")))))

(ert-deftest exercism--cli-version-self-check-result ()
  (let ((exercism-executable (exercism-ert--make-fake-cli-with-version "3.5.8")))
    (unwind-protect
        (let ((result (exercism--cli-version-self-check-result)))
          (should (string= (format "CLI version (min %s)" exercism--min-cli-version)
                           (nth 0 result)))
          (should (nth 1 result))
          (should (string= "3.5.8" (nth 2 result))))
      (when (file-exists-p exercism-executable)
        (delete-file exercism-executable)))))

(ert-deftest exercism--cli-version-self-check-result-below-min ()
  (let ((exercism-executable (exercism-ert--make-fake-cli-with-version "3.1.0")))
    (unwind-protect
        (let ((result (exercism--cli-version-self-check-result)))
          (should-not (nth 1 result))
          (should (string-match-p "below min" (nth 2 result))))
      (when (file-exists-p exercism-executable)
        (delete-file exercism-executable)))))

(ert-deftest exercism-self-check-cli-version-after-executable ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir))
         (cli (exercism-ert--make-fake-cli-with-version "3.5.8")))
    (unwind-protect
        (exercism-ert--with-valid-setup config-file workspace
         (lambda ()
           (let ((exercism-executable cli))
             (exercism-ert--run-self-check-without-async)
             (with-current-buffer "*exercism-self-check*"
               (let* ((content (buffer-string))
                      (executable-pos (string-match "CLI executable" content))
                      (version-pos (string-match "CLI version (min" content))
                      (config-pos (string-match "Config file" content)))
                 (should executable-pos)
                 (should version-pos)
                 (should config-pos)
                 (should (< executable-pos version-pos))
                 (should (< version-pos config-pos))
                 (should (string-match-p "CLI version (min 3.2.0): 3.5.8"
                                         content)))))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (file-exists-p cli) (delete-file cli))
      (when (get-buffer "*exercism-self-check*")
        (kill-buffer "*exercism-self-check*")))))

(ert-deftest exercism-self-check-mode-activation ()
  (unwind-protect
      (progn
        (exercism-ert--run-self-check-without-async)
        (with-current-buffer "*exercism-self-check*"
          (should (derived-mode-p 'exercism-self-check-mode))))
    (when (get-buffer "*exercism-self-check*")
      (kill-buffer "*exercism-self-check*"))))

(ert-deftest exercism-self-check-rerun-key ()
  (should (eq #'exercism-self-check
              (lookup-key exercism-self-check-mode-map "g"))))

(ert-deftest exercism-self-check-quit-key ()
  (should (eq #'quit-window
              (lookup-key exercism-self-check-mode-map "q"))))

(ert-deftest exercism-self-check-configure-key ()
  (should (eq #'exercism-self-check-configure
              (lookup-key exercism-self-check-mode-map "c"))))

(ert-deftest exercism-self-check-track-key ()
  (should (eq #'exercism-self-check-select-track
              (lookup-key exercism-self-check-mode-map "t"))))

(ert-deftest exercism-self-check-exercises-key ()
  (should (eq #'exercism-self-check-open-exercises
              (lookup-key exercism-self-check-mode-map "e"))))

(ert-deftest exercism--self-check-key-help-base ()
  (let ((config-file (make-temp-file "exercism-user" nil ".json"))
        (workspace (make-temp-file "exercism-workspace" 'dir))
        (cli (exercism-ert--make-fake-cli)))
    (unwind-protect
        (progn
          (delete-file config-file)
          (let ((exercism-config-path config-file)
                (exercism-executable cli)
                (exercism--workspace workspace))
            (when (boundp 'exercism--api-token)
              (makunbound 'exercism--api-token))
            (should (string= "g rerun | c configure | q quit"
                             (exercism--self-check-key-help)))))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (file-exists-p cli) (delete-file cli)))))

(ert-deftest exercism--self-check-key-help-configured ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir)))
    (unwind-protect
        (exercism-ert--with-valid-setup config-file workspace
         (lambda ()
           (setq exercism--current-track nil)
           (should (string= "g rerun | c configure | q quit | t track"
                            (exercism--self-check-key-help)))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t)))))

(ert-deftest exercism--self-check-key-help-with-track ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir)))
    (unwind-protect
        (exercism-ert--with-valid-setup config-file workspace
         (lambda ()
           (setq exercism--current-track "go")
           (should (string= "g rerun | c configure | q quit | t track | e exercises"
                            (exercism--self-check-key-help)))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t)))))

(ert-deftest exercism-self-check-shows-track-key-when-setup-ok ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir)))
    (unwind-protect
        (exercism-ert--with-valid-setup config-file workspace
         (lambda ()
           (setq exercism--current-track nil)
           (exercism-ert--run-self-check-without-async)
           (with-current-buffer "*exercism-self-check*"
             (should (string-match-p "| t track" (buffer-string)))
             (should-not (string-match-p "| e exercises" (buffer-string))))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (get-buffer "*exercism-self-check*")
        (kill-buffer "*exercism-self-check*")))))

(ert-deftest exercism-self-check-shows-exercises-key-with-track ()
  (let* ((config-file (make-temp-file "exercism-user" nil ".json"))
         (workspace (make-temp-file "exercism-workspace" 'dir)))
    (unwind-protect
        (exercism-ert--with-valid-setup config-file workspace
         (lambda ()
           (setq exercism--current-track "go")
           (exercism-ert--run-self-check-without-async)
           (with-current-buffer "*exercism-self-check*"
             (should (string-match-p "| t track" (buffer-string)))
             (should (string-match-p "| e exercises" (buffer-string))))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t))
      (when (get-buffer "*exercism-self-check*")
        (kill-buffer "*exercism-self-check*")))))

(ert-deftest exercism--configure-after-callback ()
  (let ((config-file (make-temp-file "exercism-user" nil ".json"))
        (workspace (make-temp-file "exercism-workspace" 'dir))
        (called nil))
    (unwind-protect
        (progn
          (write-region "{}" nil config-file)
          (cl-letf ((exercism-config-path config-file))
            (advice-add #'exercism--run-shell-command :around
                        #'exercism-ert--run-shell-command-immediate)
            (exercism--configure "test-token" workspace
                                 (lambda () (setq called t)))
            (should called)))
      (advice-remove #'exercism--run-shell-command
                     #'exercism-ert--run-shell-command-immediate)
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p workspace) (delete-directory workspace t)))))

(ert-deftest exercism--self-check-show-pending ()
  (unwind-protect
      (progn
        (exercism--self-check-show-pending "Configuring... (please wait)")
        (with-current-buffer "*exercism-self-check*"
          (should (derived-mode-p 'exercism-self-check-mode))
          (goto-char (point-min))
          (should (search-forward "Configuring... (please wait)" nil t))))
    (when (get-buffer "*exercism-self-check*")
      (kill-buffer "*exercism-self-check*"))))

(ert-deftest exercism-self-check-shows-key-help ()
  (unwind-protect
      (progn
        (exercism-ert--run-self-check-without-async)
        (with-current-buffer "*exercism-self-check*"
          (should (string-match-p "g rerun | c configure | q quit"
                                  (buffer-string)))))
    (when (get-buffer "*exercism-self-check*")
      (kill-buffer "*exercism-self-check*"))))

(ert-deftest exercism-track-list-initial-point ()
  "Characterize: render leaves point on the first track row."
  (exercism-ert--with-track-list
   (exercism-ert--sample-tracks) nil
   (lambda ()
     (should (equal "emacs-lisp"
                    (get-text-property (point) 'exercism-track-slug))))))

(ert-deftest exercism--submit-pending-set-starts-animation ()
  "Characterize: submitting state starts animation and updates the row."
  (unwind-protect
      (exercism-ert--with-exercise-list
       (list '((slug . "bob")
               (difficulty . "easy")
               (blurb . "Bob says hi")
               (is_unlocked . t)))
       (exercism-ert--make-solution-table nil)
       (lambda ()
         (exercism--submit-pending-set "bob" 'submitting)
         (should (eq 'submitting (gethash "bob" exercism--exercise-pending-states)))
         (should (timerp exercism--submit-animation-timer))
         (let ((line (buffer-substring-no-properties
                      (car (exercism-exercise-list--line-for-slug "bob"))
                      (cdr (exercism-exercise-list--line-for-slug "bob")))))
           (should (string-match-p "submitting" line)))))
    (exercism--submit-animation-stop)
    (clrhash exercism--exercise-pending-states)))

(ert-deftest exercism--self-check-done-decrements-pending ()
  "Characterize: each async completion decrements pending and messages at zero."
  (let ((exercism--self-check-pending 2)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (exercism--self-check-done)
      (should (= 1 exercism--self-check-pending))
      (should (null messages))
      (exercism--self-check-done)
      (should (zerop exercism--self-check-pending))
      (should (equal '("[exercism] self-check complete — see *exercism-self-check*")
                     messages)))))

(ert-deftest exercism--load-then-reconcile-order ()
  "Characterize: load-state then reconcile clears stale track/exercise."
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
            (setq exercism--current-track "stale"
                  exercism--current-exercise "stale-ex"
                  exercism--workspace "/tmp/other")
            (exercism--load-state)
            (should (equal "emacs-lisp" exercism--current-track))
            (should (equal "hello-world" exercism--current-exercise))
            (exercism--reconcile-state-with-config)
            (should (equal "go" exercism--current-track))
            (should (null exercism--current-exercise))
            (should (string= exercism--workspace workspace))))
      (when (file-exists-p config-file) (delete-file config-file))
      (when (file-exists-p state-file) (delete-file state-file))
      (when (file-exists-p workspace) (delete-directory workspace t)))))

(ert-deftest exercism--order-exercises-stable-partition ()
  "Characterize: unsolved keep relative order; solved follow stably."
  (let* ((exercises (exercism-ert--sample-exercises))
         (solutions (exercism-ert--make-solution-table
                     '(("hello-world" . "published")
                       ("two-fer" . "started")
                       ("bob" . nil))))
         (ordered (exercism--order-exercises exercises solutions)))
    (should (equal '("two-fer" "secret-handshake" "bob" "hello-world")
                   (mapcar (lambda (ex)
                             (exercism--json-value
                              (exercism--plist-get ex 'slug)))
                           ordered)))))

(provide 'exercism-tests)
;;; exercism-tests.el ends here
