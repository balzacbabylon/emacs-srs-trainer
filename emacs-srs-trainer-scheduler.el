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

(defcustom emacs-srs-trainer-scheduler-learning-steps-seconds
  '(60 600 86400)
  "Learning step delays in seconds.

This follows Anki's learning-step model for this package's binary review
buttons: a correct answer advances to the next learning step, while an
incorrect answer returns the card to the first learning step.  The default
steps are 1 minute, 10 minutes, and 1 day."
  :type '(repeat integer))

(defcustom emacs-srs-trainer-scheduler-graduating-interval-days 3
  "First review interval, in days, after a card finishes learning steps."
  :type 'integer)

(defcustom emacs-srs-trainer-scheduler-lapse-delay-seconds 60
  "Legacy fallback delay before an incorrectly answered card is due again.

When `emacs-srs-trainer-scheduler-learning-steps-seconds' is non-empty,
the first learning step controls incorrect-answer delay instead."
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
          :learning-step nil
          :graduated nil
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

(defun emacs-srs-trainer-scheduler--learning-steps ()
  "Return configured positive learning steps in seconds."
  (delq nil
        (mapcar (lambda (seconds)
                  (when (and (integerp seconds) (> seconds 0))
                    seconds))
                emacs-srs-trainer-scheduler-learning-steps-seconds)))

(defun emacs-srs-trainer-scheduler--first-learning-delay ()
  "Return first learning-step delay in seconds."
  (or (car (emacs-srs-trainer-scheduler--learning-steps))
      emacs-srs-trainer-scheduler-lapse-delay-seconds))

(defun emacs-srs-trainer-scheduler--legacy-review-state-p (state)
  "Return non-nil when STATE looks graduated under pre-step storage."
  (and (not (plist-member state :graduated))
       (not (plist-member state :learning-step))
       (>= (or (plist-get state :repetition-count) 0) 2)))

(defun emacs-srs-trainer-scheduler-card-type (state)
  "Return Anki-style card type symbol for scheduler STATE.

The result is one of `new', `learning', or `review'.  Lapsed cards
that are going through this package's relearning path are reported as
`learning', matching the three queue labels shown during study."
  (cond
   ((null (plist-get state :last-reviewed)) 'new)
   ((or (plist-get state :graduated)
        (emacs-srs-trainer-scheduler--legacy-review-state-p state))
    'review)
   ((integerp (plist-get state :learning-step))
    'learning)
   (t 'learning)))

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
         (lapses (or (plist-get old-state :lapse-count) 0))
         (steps (emacs-srs-trainer-scheduler--learning-steps))
         (card-type (emacs-srs-trainer-scheduler-card-type old-state)))
    (if correct
        (let ((new-ease (max emacs-srs-trainer-scheduler-minimum-ease
                             (+ ease 0.05))))
          (if (eq card-type 'review)
              (let* ((new-reps (1+ reps))
                     (new-interval
                      (max 1 (round (* (max 1 interval) new-ease)))))
                (list :ease new-ease
                      :interval new-interval
                      :repetition-count new-reps
                      :learning-step nil
                      :graduated t
                      :lapse-count lapses
                      :due (+ timestamp (emacs-srs-trainer-scheduler--days-to-seconds
                                         new-interval))
                      :last-reviewed timestamp
                      :last-result 'correct))
            (let* ((current-step (or (plist-get old-state :learning-step) 0))
                   (next-step (if (null (plist-get old-state :last-reviewed))
                                  (min 1 (max 0 (1- (length steps))))
                                (1+ current-step))))
              (if (and steps (< next-step (length steps)))
                  (list :ease new-ease
                        :interval 0
                        :repetition-count (1+ reps)
                        :learning-step next-step
                        :graduated nil
                        :lapse-count lapses
                        :due (+ timestamp (nth next-step steps))
                        :last-reviewed timestamp
                        :last-result 'correct)
                (let ((new-interval emacs-srs-trainer-scheduler-graduating-interval-days))
                  (list :ease new-ease
                        :interval new-interval
                        :repetition-count (1+ reps)
                        :learning-step nil
                        :graduated t
                        :lapse-count lapses
                        :due (+ timestamp (emacs-srs-trainer-scheduler--days-to-seconds
                                           new-interval))
                        :last-reviewed timestamp
                        :last-result 'correct))))))
      (let ((new-ease (max emacs-srs-trainer-scheduler-minimum-ease
                           (- ease 0.2))))
        (list :ease new-ease
              :interval 0
              :repetition-count 0
              :learning-step 0
              :graduated nil
              :lapse-count (1+ lapses)
              :due (+ timestamp (emacs-srs-trainer-scheduler--first-learning-delay))
              :last-reviewed timestamp
              :last-result 'incorrect)))))

(provide 'emacs-srs-trainer-scheduler)

;;; emacs-srs-trainer-scheduler.el ends here
