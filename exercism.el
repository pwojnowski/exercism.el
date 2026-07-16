;;; exercism.el --- Exercism.org CLI integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: Rafael Nicdao
;; Version: 1.0.0
;; Keywords: exercism, convenience
;; Homepage: https://github.com/anonimitoraf/exercism.el
;; Package-Requires: ((emacs "29.1") (request "0.3.2"))

;;; Commentary:

;; Do Exercism exercises within Emacs via the `exercism' CLI.
;; Entry point: `M-x exercism' or `C-c x' (exercise list).

;;; Code:

(require 'exercism-core)
(require 'exercism-cli)
(require 'exercism-api)
(require 'exercism-list)
(require 'exercism-track-list)
(require 'exercism-exercise-list)
(require 'exercism-self-check)

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

(exercism--load-state)
(exercism--reconcile-state-with-config)

(provide 'exercism)
;;; exercism.el ends here
