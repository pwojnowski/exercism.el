;;; exercism-ert-bootstrap.el --- Batch bootstrap for exercism ERT -*- lexical-binding: t; -*-

;;; Commentary:

;; Minimal bootstrap for running exercism ERT tests without loading init.el.
;; Invoked by scripts/run-exercism-ert.sh.

;;; Code:

(let* ((repo-root (file-name-directory load-file-name))
       (emacs-dir (expand-file-name (or (getenv "EMACS_USER_DIR") "~/.emacs.d")))
       (elpa-dir (expand-file-name "elpa" emacs-dir)))
  (setq load-prefer-newer t)
  (add-to-list 'load-path repo-root)
  (when (file-directory-p elpa-dir)
    (dolist (dir (directory-files elpa-dir t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir))))
  (setq exercism--state-file (make-temp-file "exercism-ert-state" nil ".el")))

(defun exercism-ert-bootstrap--require (feature)
  "Require FEATURE or signal a helpful error."
  (condition-case err
      (require feature)
    (error
     (error "Cannot load `%s' for exercism ERT: %s\nInstall dependencies from Emacs with package-initialize, then M-x package-install RET %s RET"
            feature (error-message-string err) feature))))

(exercism-ert-bootstrap--require 'ert)
(exercism-ert-bootstrap--require 'request)
(exercism-ert-bootstrap--require 'transient)
(exercism-ert-bootstrap--require 'exercism)
(exercism-ert-bootstrap--require 'exercism-ert)

(ert-run-tests-batch-and-exit)
;;; exercism-ert-bootstrap.el ends here
