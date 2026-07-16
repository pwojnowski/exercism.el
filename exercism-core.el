;;; exercism-core.el --- Core state and helpers for exercism.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Customization, persisted state, workspace/config access, and JSON
;; normalization helpers shared by the rest of exercism.el.

;;; Code:

(require 'cl-lib)
(require 'json)
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

(defvar exercism--api-token nil
  "Cached Exercism API token loaded from the CLI user config.")
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

(defun exercism--file-to-string (file-path)
  "Return the contents of FILE-PATH as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

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

(defun exercism--plist-get (plist key)
  "Return KEY from PLIST, an alist, or a mixed JSON object."
  (let ((sym (if (symbolp key) key (intern (format "%s" key)))))
    (or (plist-get plist sym)
        (plist-get plist key)
        (alist-get sym plist nil nil #'equal)
        (alist-get key plist nil nil #'equal))))

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

(defun exercism--ensure-current-track ()
  "Signal an error unless `exercism--current-track' is set."
  (unless exercism--current-track
    (user-error "Set a track first (`t' in the exercise list, or `M-x exercism-exercise-list-set-track')")))

(defun exercism--set-current-track (track)
  "Set TRACK as current, persist state, and message."
  (setq exercism--current-track track)
  (exercism--save-state)
  (message "[exercism] set current track to: %s" track))

(defun exercism--exercise-dir-for-slug (slug)
  "Return the absolute directory for SLUG on `exercism--current-track'."
  (expand-file-name slug
                    (expand-file-name exercism--current-track
                                      exercism--workspace)))

(provide 'exercism-core)
;;; exercism-core.el ends here
