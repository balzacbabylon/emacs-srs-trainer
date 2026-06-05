;;; emacs-srs-trainer-scheduler.el --- Spaced repetition scheduler  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A small SM-2-style scheduler behind a narrow interface so it can be
;; replaced later without changing the review UI or storage layer.

;;; Code:

(require 'cl-lib)

(defgroup emacs-srs-trainer-scheduler nil
  "Scheduler settings for `emacs-srs-trainer'."
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-scheduler-initial-ease 2.5
  "Initial ease factor for new cards."
  :type 'number)

(defcustom emacs-srs-trainer-scheduler-minimum-ease 1.3
  "Minimum ease factor."
  :type 'number)

(defcustom emacs-srs-trainer-scheduler-first-interval-days 1
  "First correct interval, in days."
  :type 'integer)

(defcustom emacs-srs-trainer-scheduler-second-interval-days 3
  "Second correct interval, in days."
  :type 'integer)

(defcustom emacs-srs-trainer-scheduler-lapse-delay-seconds 60
  "Delay before an incorrectly answered card is due again."
  :type 'integer)

(defcustom emacs-srs-trainer-scheduler-learning-graduation-repetitions 2
  "Correct reviews needed before a learning card becomes a review card."
  :type 'integer)

(defconst emacs-srs-trainer-scheduler-card-types
  '(new learning review)
  "Supported card queue types.")

(defun emacs-srs-trainer-scheduler-now ()
  "Return the current timestamp as a float."
  (float-time (current-time)))

(defun emacs-srs-trainer-scheduler-new-state (&optional now)
  "Return scheduler state for a new card due at NOW."
  (let ((timestamp (or now (emacs-srs-trainer-scheduler-now))))
    (list :ease emacs-srs-trainer-scheduler-initial-ease
          :interval 0
          :repetition-count 0
          :lapse-count 0
          :due timestamp
          :last-reviewed nil
          :last-result nil)))

(defun emacs-srs-trainer-scheduler-due-p (state &optional now)
  "Return non-nil when STATE is due at NOW."
  (let ((due (plist-get state :due))
        (timestamp (or now (emacs-srs-trainer-scheduler-now))))
    (or (null due) (<= due timestamp))))

(defun emacs-srs-trainer-scheduler--days-to-seconds (days)
  "Convert DAYS to seconds."
  (* days 24 60 60))

(defun emacs-srs-trainer-scheduler-card-type (state)
  "Return Anki-style card type symbol for scheduler STATE.

The result is one of `new', `learning', or `review'.  Lapsed cards
that are going through this package's relearning path are reported as
`learning', matching the three queue labels shown during study."
  (cond
   ((null (plist-get state :last-reviewed)) 'new)
   ((< (or (plist-get state :repetition-count) 0)
       emacs-srs-trainer-scheduler-learning-graduation-repetitions)
    'learning)
   (t 'review)))

(defun emacs-srs-trainer-scheduler-card-type-label (type)
  "Return user-facing label for card TYPE."
  (pcase type
    ('new "New")
    ('learning "Learning")
    ('review "To Review")
    (_ "Unknown")))

(defun emacs-srs-trainer-scheduler-card-type-sort-key (type)
  "Return study-order sort key for card TYPE."
  (pcase type
    ('learning 0)
    ('review 1)
    ('new 2)
    (_ 3)))

(defun emacs-srs-trainer-scheduler-review (state correct &optional now)
  "Return updated scheduler STATE after review result CORRECT.

CORRECT should be non-nil for a remembered answer and nil for a
forgotten answer."
  (let* ((timestamp (or now (emacs-srs-trainer-scheduler-now)))
         (old-state (or state (emacs-srs-trainer-scheduler-new-state timestamp)))
         (ease (or (plist-get old-state :ease)
                   emacs-srs-trainer-scheduler-initial-ease))
         (interval (or (plist-get old-state :interval) 0))
         (reps (or (plist-get old-state :repetition-count) 0))
         (lapses (or (plist-get old-state :lapse-count) 0)))
    (if correct
        (let* ((new-reps (1+ reps))
               (new-ease (max emacs-srs-trainer-scheduler-minimum-ease
                              (+ ease 0.05)))
               (new-interval
                (cond
                 ((= new-reps 1) emacs-srs-trainer-scheduler-first-interval-days)
                 ((= new-reps 2) emacs-srs-trainer-scheduler-second-interval-days)
                 (t (max 1 (round (* (max 1 interval) new-ease)))))))
          (list :ease new-ease
                :interval new-interval
                :repetition-count new-reps
                :lapse-count lapses
                :due (+ timestamp (emacs-srs-trainer-scheduler--days-to-seconds
                                   new-interval))
                :last-reviewed timestamp
                :last-result 'correct))
      (let ((new-ease (max emacs-srs-trainer-scheduler-minimum-ease
                           (- ease 0.2))))
        (list :ease new-ease
              :interval 0
              :repetition-count 0
              :lapse-count (1+ lapses)
              :due (+ timestamp emacs-srs-trainer-scheduler-lapse-delay-seconds)
              :last-reviewed timestamp
              :last-result 'incorrect)))))

(provide 'emacs-srs-trainer-scheduler)

;;; emacs-srs-trainer-scheduler.el ends here
