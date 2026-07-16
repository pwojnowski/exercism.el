;;; exercism-api.el --- Exercism HTTP API boundary -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Rafael Nicdao
;; Copyright (C) 2026 Przemysław Wojnowski
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Authentication headers and Exercism.org HTTP API requests.

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'url)
(require 'exercism-core)

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

(defun exercism--request-error-message (error-thrown response)
  "Format ERROR-THROWN and RESPONSE into a user-facing error string."
  (or (when response
        (format "HTTP %s"
                (request-response-status-code response)))
      (format "%s" error-thrown)))

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
                          (exercism--request-error-message
                           error-thrown response)))))))
    (apply #'request url
           :parser #'json-read
           :success success
           :error error-handler
           (when headers (list :headers headers)))))

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

(defun exercism--solutions-ingest-page (solutions data)
  "Store solution statuses from DATA into SOLUTIONS hash table."
  (let ((results (exercism--plist-get data 'results)))
    (seq-doseq (solution results)
      (let* ((exercise (exercism--plist-get solution 'exercise))
             (slug (exercism--plist-get exercise 'slug)))
        (when slug
          (puthash (exercism--json-value slug)
                   (exercism--plist-get solution 'status)
                   solutions))))))

(defun exercism--solutions-page-meta (data page)
  "Return (CURRENT-PAGE . TOTAL-PAGES) from DATA, defaulting to PAGE."
  (let* ((meta (exercism--plist-get data 'meta))
         (current-page (or (exercism--plist-get meta 'current_page) page))
         (total-pages (or (exercism--plist-get meta 'total_pages) page)))
    (cons current-page total-pages)))

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
                        (exercism--solutions-ingest-page solutions data)
                        (let* ((meta (exercism--solutions-page-meta data page))
                               (current-page (car meta))
                               (total-pages (cdr meta)))
                          (if (< current-page total-pages)
                              (fetch-page (1+ current-page))
                            (funcall callback solutions)))))
            :error (cl-function
                    (lambda (&key error-thrown response &allow-other-keys)
                      (user-error "Failed to fetch solutions: %s"
                                  (exercism--request-error-message
                                   error-thrown response)))))))
      (fetch-page 1))))

(provide 'exercism-api)
;;; exercism-api.el ends here
