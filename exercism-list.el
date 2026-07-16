;;; exercism-list.el --- Shared list buffer primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Property-based row navigation and shared rendering helpers for
;; Exercism list buffers.

;;; Code:

(require 'exercism-core)

(defun exercism--list-row-p (property)
  "Return non-nil when point is on a row with text PROPERTY."
  (get-text-property (point) property))

(defun exercism--list-slug-at-point (property)
  "Return the PROPERTY value at point when on a row, otherwise nil."
  (and (exercism--list-row-p property)
       (get-text-property (point) property)))

(defun exercism--list-goto-next-row (property)
  "Move point to the next row marked by PROPERTY, if any."
  (let ((next (next-single-property-change (point) property)))
    (when next
      (goto-char next)
      (unless (exercism--list-row-p property)
        (exercism--list-goto-next-row property)))))

(defun exercism--list-goto-previous-row (property)
  "Move point to the previous row marked by PROPERTY, if any."
  (let ((prev (previous-single-property-change (point) property)))
    (when prev
      (goto-char prev)
      (unless (exercism--list-row-p property)
        (exercism--list-goto-previous-row property)))))

(defun exercism--list-goto-first-row (property)
  "Move point to the first row marked by PROPERTY."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (get-text-property (point) property)
        (throw 'found t))
      (forward-line 1))))

(defun exercism--list-longest-field (items property)
  "Return the longest PROPERTY string length among ITEMS."
  (apply #'max 0
         (mapcar (lambda (item)
                   (length (exercism--json-value
                            (exercism--plist-get item property))))
                 items)))

(defun exercism--list-insert-heading (title key-help)
  "Insert TITLE underline, KEY-HELP, and trailing blank line."
  (insert title "\n")
  (insert (make-string (length title) ?=) "\n\n")
  (insert key-help "\n\n"))

(provide 'exercism-list)
;;; exercism-list.el ends here
