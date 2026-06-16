;;; emacs-srs-trainer.el --- Emacs-native SRS trainer for tutorial keys  -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: emacs-srs-trainer contributors
;; Maintainer: emacs-srs-trainer contributors
;; Version: 0.1.0
;; URL: https://github.com/balzacbabylon/emacs-srs-trainer
;; Keywords: learning, convenience
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Run `M-x emacs-srs-trainer-review' to review Emacs Tutorial
;; keybindings by pressing the real key sequences inside Emacs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'emacs-srs-trainer-deck)
(require 'emacs-srs-trainer-scheduler)
(require 'emacs-srs-trainer-storage)
(require 'emacs-srs-trainer-tutorial)
(require 'emacs-srs-trainer-info)
(require 'emacs-srs-trainer-org)
(require 'emacs-srs-trainer-validate)

(defcustom emacs-srs-trainer-review-buffer-name "*Emacs SRS Trainer*"
  "Buffer name for reviews."
  :type 'string
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-default-deck emacs-srs-trainer-tutorial-deck-name
  "Default deck used by `emacs-srs-trainer-review'."
  :type 'string
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-shuffle-cards-within-queue t
  "When non-nil, shuffle cards inside each due queue.

Queue buckets still appear in scheduler order: Learning, To Review,
then New.  This option only randomizes the order of cards within each
bucket so cards that graduate or come due together do not reappear in
deck order."
  :type 'boolean
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-continuation-debounce-seconds 0.08
  "Seconds to ignore repeated continuation key events after moving on.

This prevents a held or double-tapped `SPC' or `RET' from being consumed
as the next card's answer immediately after it was used to leave the
previous result screen.  Set this to 0 to disable debouncing."
  :type 'number
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-answer-feedback-delay-seconds 0.45
  "Seconds to keep final answer feedback visible in the echo area.

When an answer is completed, the echo area briefly flashes the typed
sequence green for correct answers or the first wrong event red for
incorrect answers.  Set this to 0 to leave the echo area unchanged."
  :type 'number
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-review-card-limit nil
  "Maximum number of due cards to review in one session.

When nil, review every due card.  This only limits the size of a review
session; it does not change stored due timestamps.  The value can also
be overridden for one command with a numeric prefix argument."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Cards per review session"))
  :group 'emacs-srs-trainer)

(defface emacs-srs-trainer-correct-face
  '((t :foreground "green3" :weight bold))
  "Face used for correct answers."
  :group 'emacs-srs-trainer)

(defface emacs-srs-trainer-incorrect-face
  '((t :foreground "red3" :weight bold))
  "Face used for incorrect answers."
  :group 'emacs-srs-trainer)

(defvar emacs-srs-trainer-read-answer-function
  #'emacs-srs-trainer-read-answer-key-sequence
  "Function used by the review loop to capture an answer.")

(defvar emacs-srs-trainer-read-continuation-function
  #'emacs-srs-trainer-read-continuation
  "Function used by the review loop after grading.")

(defvar emacs-srs-trainer-read-completion-action-function
  #'emacs-srs-trainer-read-completion-action
  "Function used by the review loop after a session completes.")

(defvar emacs-srs-trainer-current-card nil
  "Card currently being answered by the review loop.")

(defvar emacs-srs-trainer--last-continuation-event nil
  "Raw event most recently used as a review continuation key.")

(defvar emacs-srs-trainer--answer-feedback-clear-timer nil
  "Timer used to clear answer feedback from the echo area.")

(define-derived-mode emacs-srs-trainer-review-mode fundamental-mode "Emacs-SRS"
  "Major mode for the Emacs SRS review buffer."
  (setq-local cursor-type nil)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil))

(define-derived-mode emacs-srs-trainer-welcome-mode special-mode "Emacs-SRS-Welcome"
  "Major mode for the Emacs SRS Trainer welcome buffer."
  (setq-local truncate-lines nil))

(define-key emacs-srs-trainer-welcome-mode-map (kbd "TAB") #'forward-button)
(define-key emacs-srs-trainer-welcome-mode-map (kbd "<backtab>") #'backward-button)

(defun emacs-srs-trainer--prefix-argument-command-p (sequence)
  "Return non-nil when SEQUENCE invokes a prefix-argument command."
  (memq (key-binding sequence t)
        '(universal-argument universal-argument-more digit-argument negative-argument)))

(defun emacs-srs-trainer--prefix-argument-continuation-p (sequence)
  "Return non-nil when SEQUENCE continues a prefix argument."
  (let ((description (emacs-srs-trainer-normalize-key sequence)))
    (or (emacs-srs-trainer--prefix-argument-command-p sequence)
        (string-match-p (rx bos (+ digit) eos) description)
        (member description '("-" "M--")))))

(defun emacs-srs-trainer--key-to-vector (key)
  "Return KEY as a vector suitable for prefix comparison."
  (cond
   ((vectorp key) key)
   ((stringp key) (vconcat (kbd key)))
   (t (error "Unsupported key value: %S" key))))

(defun emacs-srs-trainer--vector-prefix-p (prefix vector)
  "Return non-nil when PREFIX is an event prefix of VECTOR."
  (and (<= (length prefix) (length vector))
       (cl-loop for index from 0 below (length prefix)
                always (equal (aref prefix index)
                              (aref vector index)))))

(defun emacs-srs-trainer--token-prefix-p (prefix-description description)
  "Return non-nil when PREFIX-DESCRIPTION is a key-token prefix of DESCRIPTION."
  (let ((prefix-tokens (split-string prefix-description " " t))
        (tokens (split-string description " " t)))
    (and (<= (length prefix-tokens) (length tokens))
         (cl-loop for prefix-token in prefix-tokens
                  for token in tokens
                  always (string= prefix-token token)))))

(defun emacs-srs-trainer--answer-prefix-p (events answer-vectors normalized-answers)
  "Return non-nil when EVENTS can still become one of the accepted answers."
  (let ((description (emacs-srs-trainer-normalize-key events)))
    (or (cl-some (lambda (answer-vector)
                   (emacs-srs-trainer--vector-prefix-p events answer-vector))
                 answer-vectors)
        (cl-some (lambda (answer)
                   (emacs-srs-trainer--token-prefix-p description answer))
                 normalized-answers))))

(defun emacs-srs-trainer--answer-display-tokens (events)
  "Return normalized display tokens for EVENTS."
  (split-string (emacs-srs-trainer-normalize-key events) " " t))

(defun emacs-srs-trainer--propertize-answer-tokens (tokens face)
  "Return TOKENS with FACE applied to every token."
  (mapcar (lambda (token)
            (propertize token 'face face))
          tokens))

(defun emacs-srs-trainer--answer-feedback-message (tokens)
  "Show TOKENS as the current answer in the echo area."
  (emacs-srs-trainer--answer-feedback-cancel-clear-timer)
  (message "%s"
           (concat "Answer: "
                   (emacs-srs-trainer-display-key-tokens tokens))))

(defun emacs-srs-trainer--answer-feedback-prompt (events)
  "Return an answer prompt showing EVENTS typed so far."
  (concat "Answer: "
          (when (> (length events) 0)
            (concat
             (emacs-srs-trainer-display-key-tokens
              (emacs-srs-trainer--answer-display-tokens events))
             " "))))

(defun emacs-srs-trainer--answer-feedback-cancel-clear-timer ()
  "Cancel any pending answer feedback clear timer."
  (when (timerp emacs-srs-trainer--answer-feedback-clear-timer)
    (cancel-timer emacs-srs-trainer--answer-feedback-clear-timer))
  (setq emacs-srs-trainer--answer-feedback-clear-timer nil))

(defun emacs-srs-trainer--answer-feedback-schedule-clear ()
  "Schedule final answer feedback to clear without blocking review flow."
  (emacs-srs-trainer--answer-feedback-cancel-clear-timer)
  (when (and (not noninteractive)
             emacs-srs-trainer-answer-feedback-delay-seconds
             (> emacs-srs-trainer-answer-feedback-delay-seconds 0))
    (setq emacs-srs-trainer--answer-feedback-clear-timer
          (run-at-time
           emacs-srs-trainer-answer-feedback-delay-seconds
           nil
           (lambda ()
             (setq emacs-srs-trainer--answer-feedback-clear-timer nil)
             (message nil))))))

(defun emacs-srs-trainer--echo-answer-progress (events)
  "Echo current answer EVENTS without final grading feedback."
  (emacs-srs-trainer--answer-feedback-message
   (emacs-srs-trainer--answer-display-tokens events)))

(defun emacs-srs-trainer--echo-correct-answer (events)
  "Flash completed answer EVENTS as correct."
  (emacs-srs-trainer--answer-feedback-message
   (emacs-srs-trainer--propertize-answer-tokens
    (emacs-srs-trainer--answer-display-tokens events)
    'emacs-srs-trainer-correct-face))
  (emacs-srs-trainer--answer-feedback-schedule-clear))

(defun emacs-srs-trainer--echo-incorrect-answer (previous-events event)
  "Flash EVENT as the first incorrect input after PREVIOUS-EVENTS."
  (let ((tokens (append
                 (emacs-srs-trainer--answer-display-tokens previous-events)
                 (emacs-srs-trainer--propertize-answer-tokens
                  (emacs-srs-trainer--answer-display-tokens (vector event))
                  'emacs-srs-trainer-incorrect-face))))
    (emacs-srs-trainer--answer-feedback-message tokens)
    (emacs-srs-trainer--answer-feedback-schedule-clear)))

(defun emacs-srs-trainer--read-answer-key-sequence-dispatch ()
  "Read one complete Emacs key sequence without card-aware early grading."
  (let ((events [])
        (done nil)
        (reading-prefix nil)
        (old-quit-flag quit-flag))
    (unwind-protect
        (let ((inhibit-quit t)
              (quit-flag nil)
              (overriding-terminal-local-map nil)
              (overriding-local-map nil)
              (overriding-local-map-menu-flag nil)
              (special-event-map special-event-map))
          (while (not done)
            (let ((sequence (read-key-sequence-vector
                             "Answer key sequence: ")))
              (setq events (vconcat events sequence))
              (if (or (emacs-srs-trainer--prefix-argument-command-p sequence)
                      (and reading-prefix
                           (emacs-srs-trainer--prefix-argument-continuation-p sequence)))
                  (setq reading-prefix t)
                (setq done t))))
          (emacs-srs-trainer-normalize-key events))
      (setq quit-flag old-quit-flag))))

(defun emacs-srs-trainer--read-answer-key-sequence-for-card (card)
  "Read an answer for CARD event by event.

Reading stops as soon as the captured events equal an accepted answer
or can no longer be a prefix of one.  This makes wrong answers report
immediately: if the card expects `C-x C-f' and the user starts with
`C-g', capture returns `C-g' without waiting for another key."
  (let* ((answers (delq nil (emacs-srs-trainer-card-answers card)))
         (answer-vectors (mapcar #'emacs-srs-trainer--key-to-vector answers))
         (normalized-answers (emacs-srs-trainer-card-normalized-answers card))
         (events [])
         (done nil)
         (old-quit-flag quit-flag))
    (unwind-protect
        (let ((inhibit-quit t)
              (quit-flag nil)
              (overriding-terminal-local-map nil)
              (overriding-local-map nil)
              (overriding-local-map-menu-flag nil)
              (special-event-map special-event-map))
          (while (not done)
            (let ((previous-events events)
                  (event (read-event
                          (emacs-srs-trainer--answer-feedback-prompt
                           events))))
              (setq events (vconcat events (vector event)))
              (let ((description (emacs-srs-trainer-normalize-key events)))
                (cond
                 ((member description normalized-answers)
                  (emacs-srs-trainer--echo-correct-answer events)
                  (setq done t))
                 ((emacs-srs-trainer--answer-prefix-p
                   events answer-vectors normalized-answers)
                  (emacs-srs-trainer--echo-answer-progress events)
                  nil)
                 (t
                  (emacs-srs-trainer--echo-incorrect-answer
                   previous-events event)
                  (setq done t))))))
          (emacs-srs-trainer-normalize-key events))
      (setq quit-flag old-quit-flag))))

;;;###autoload
(defun emacs-srs-trainer-read-answer-key-sequence (&optional card)
  "Read one answer as actual Emacs input.

When CARD, or `emacs-srs-trainer-current-card', is non-nil, read
events one at a time and stop as soon as the entered prefix cannot
match the card's canonical answer or accepted alternatives.  The
captured keys are never dispatched as commands.  `C-g' is captured
under `inhibit-quit' and does not quit the review session.

When no card is available, fall back to reading a complete Emacs key
sequence with `read-key-sequence-vector'."
  (if-let* ((answer-card (or card emacs-srs-trainer-current-card)))
      (emacs-srs-trainer--read-answer-key-sequence-for-card answer-card)
    (emacs-srs-trainer--read-answer-key-sequence-dispatch)))

(defun emacs-srs-trainer-read-continuation ()
  "Read a post-grading continuation key.

Return one of the symbols `next', `quit', or `help'."
  (let ((key (read-key "RET/SPC next, q quit, ? help: ")))
    (setq emacs-srs-trainer--last-continuation-event key)
    (cond
     ((or (eq key ?\r) (eq key ?\s) (eq key ?\n)) 'next)
     ((eq key ?q) 'quit)
     ((eq key ??) 'help)
     (t 'next))))

(defun emacs-srs-trainer-read-completion-action ()
  "Read a key after a review session completes.

Return one of the symbols `menu' or `quit'."
  (let ((key (read-key "m main menu, q quit: ")))
    (cond
     ((eq key ?m) 'menu)
     ((eq key ?q) 'quit)
     (t 'quit))))

(defun emacs-srs-trainer--event-description (event)
  "Return a normalized key description for single EVENT."
  (condition-case nil
      (emacs-srs-trainer-normalize-key (vector event))
    (error nil)))

(defun emacs-srs-trainer--read-event-timeout (seconds)
  "Read one event with timeout SECONDS."
  (read-event nil nil seconds))

(defun emacs-srs-trainer--debounce-continuation-event (event)
  "Discard repeated copies of continuation EVENT for a short interval.

Any different event read during the debounce interval is pushed back onto
`unread-command-events' so it can be used as the next answer."
  (when-let* ((seconds emacs-srs-trainer-continuation-debounce-seconds)
              ((> seconds 0))
              (target (emacs-srs-trainer--event-description event)))
    (let ((deadline (+ (float-time) seconds))
          (done nil))
      (while (and (not done)
                  (> deadline (float-time)))
        (let ((next-event
               (emacs-srs-trainer--read-event-timeout
                (- deadline (float-time)))))
          (cond
           ((null next-event)
            (setq done t))
           ((string= target (or (emacs-srs-trainer--event-description
                                 next-event)
                                ""))
            nil)
           (t
            (push next-event unread-command-events)
            (setq done t))))))))

(defun emacs-srs-trainer--state-for-card (card state now)
  "Return scheduler state for CARD from storage STATE at NOW."
  (let ((stored (emacs-srs-trainer-storage-card-state
                 (emacs-srs-trainer-card-id card) state now)))
    (or stored (emacs-srs-trainer-scheduler-new-state now))))

(defun emacs-srs-trainer-card-type (card &optional state now)
  "Return Anki-style queue type for CARD.

The result is one of `new', `learning', or `review'."
  (let* ((timestamp (or now (emacs-srs-trainer-scheduler-now)))
         (loaded-state (or state (emacs-srs-trainer-storage-load))))
    (emacs-srs-trainer-scheduler-card-type
     (emacs-srs-trainer--state-for-card card loaded-state timestamp))))

(defun emacs-srs-trainer-card-type-label (card &optional state now)
  "Return user-facing queue label for CARD."
  (emacs-srs-trainer-scheduler-card-type-label
   (emacs-srs-trainer-card-type card state now)))

(defun emacs-srs-trainer--empty-queue-counts ()
  "Return empty card queue counts."
  (list :new 0 :learning 0 :review 0 :total 0))

(defun emacs-srs-trainer--increment-queue-count (counts type)
  "Increment COUNTS for card queue TYPE and total count."
  (plist-put counts :total (1+ (or (plist-get counts :total) 0)))
  (plist-put counts
             (intern (format ":%s" (symbol-name type)))
             (1+ (or (plist-get counts
                                 (intern (format ":%s" (symbol-name type))))
                     0)))
  counts)

(defun emacs-srs-trainer--queue-counts-for-cards (cards state now &optional due-only)
  "Return queue counts for CARDS using STATE at NOW.

When DUE-ONLY is non-nil, count only cards due at NOW."
  (let ((counts (emacs-srs-trainer--empty-queue-counts)))
    (dolist (card cards counts)
      (let* ((card-state (emacs-srs-trainer--state-for-card card state now))
             (type (emacs-srs-trainer-scheduler-card-type card-state)))
        (when (or (not due-only)
                  (emacs-srs-trainer-scheduler-due-p card-state now))
          (setq counts
                (emacs-srs-trainer--increment-queue-count counts type)))))))

(defun emacs-srs-trainer-format-queue-counts (counts)
  "Format Anki-style queue COUNTS for display."
  (format "New: %d    Learning: %d    To Review: %d"
          (or (plist-get counts :new) 0)
          (or (plist-get counts :learning) 0)
          (or (plist-get counts :review) 0)))

(defun emacs-srs-trainer-format-delay (seconds)
  "Return a compact human-readable delay for SECONDS."
  (let ((rounded (max 0 (round seconds))))
    (cond
     ((< rounded 60)
      (format "%d second%s" rounded (if (= rounded 1) "" "s")))
     ((< rounded 3600)
      (let ((minutes (ceiling (/ rounded 60.0))))
        (format "%d minute%s" minutes (if (= minutes 1) "" "s"))))
     ((< rounded 86400)
      (let ((hours (ceiling (/ rounded 3600.0))))
        (format "%d hour%s" hours (if (= hours 1) "" "s"))))
     (t
      (let ((days (ceiling (/ rounded 86400.0))))
        (format "%d day%s" days (if (= days 1) "" "s")))))))

(defun emacs-srs-trainer-queue-counts (&optional topic now state due-only deck)
  "Return queue counts for DECK, defaulting to the default deck.

When TOPIC is non-nil, count only cards whose topic matches it.  When
DUE-ONLY is non-nil, count only cards due at NOW."
  (let* ((timestamp (or now (emacs-srs-trainer-scheduler-now)))
         (loaded-state (or state (emacs-srs-trainer-storage-load)))
         (deck-name (or deck emacs-srs-trainer-default-deck))
         (cards (cl-remove-if-not
                 (lambda (card)
                   (or (null topic)
                       (string= (emacs-srs-trainer-card-topic card) topic)))
                 (emacs-srs-trainer-deck-by-name
                  deck-name))))
    (emacs-srs-trainer--queue-counts-for-cards
     cards loaded-state timestamp due-only)))

(defun emacs-srs-trainer-due-counts (&optional topic now state deck)
  "Return due queue counts for DECK, defaulting to the default deck.

When TOPIC is non-nil, count only cards whose topic matches it."
  (emacs-srs-trainer-queue-counts topic now state t deck))

(defun emacs-srs-trainer--normalize-card-limit (limit)
  "Return LIMIT as a positive integer, or nil for no limit."
  (when (and (integerp limit) (> limit 0))
    limit))

(defun emacs-srs-trainer--limit-cards (cards limit)
  "Return at most LIMIT CARDS.

When LIMIT is nil or non-positive, return CARDS unchanged."
  (let ((normalized-limit (emacs-srs-trainer--normalize-card-limit limit)))
    (if (and normalized-limit (< normalized-limit (length cards)))
        (cl-subseq cards 0 normalized-limit)
      cards)))

(defun emacs-srs-trainer--read-card-limit-from-prefix (prefix &optional all-default)
  "Return a per-command card limit from PREFIX.

With nil PREFIX, use `emacs-srs-trainer-review-card-limit' unless
ALL-DEFAULT is non-nil.  With a numeric prefix, use that positive
number.  With plain `C-u', prompt for a number; 0 means no limit."
  (cond
   ((null prefix)
    (unless all-default
      (emacs-srs-trainer--normalize-card-limit
       emacs-srs-trainer-review-card-limit)))
   ((consp prefix)
    (let ((limit (read-number "Review how many cards? (0 for all) " 0)))
      (emacs-srs-trainer--normalize-card-limit limit)))
   (t
    (emacs-srs-trainer--normalize-card-limit
     (prefix-numeric-value prefix)))))

(defun emacs-srs-trainer--read-card-limit-value ()
  "Read a card limit interactively.  Zero means no limit."
  (let ((current (or emacs-srs-trainer-review-card-limit 0)))
    (emacs-srs-trainer--normalize-card-limit
     (read-number "Cards per due review session (0 for all): " current))))

(defun emacs-srs-trainer--session-limit-message (limit)
  "Return a user-facing message for LIMIT."
  (if (emacs-srs-trainer--normalize-card-limit limit)
      (format "%d" limit)
    "all due cards"))

(defun emacs-srs-trainer--all-cards-prefix-p (prefix)
  "Return non-nil when PREFIX requests all cards for practice."
  (and prefix (not (numberp prefix))))

(defun emacs-srs-trainer--shuffle-list (items)
  "Return a shuffled copy of ITEMS."
  (let ((vector (vconcat items)))
    (cl-loop for index downfrom (1- (length vector)) to 1
             do (let* ((swap-index (random (1+ index)))
                       (current (aref vector index)))
                  (aset vector index (aref vector swap-index))
                  (aset vector swap-index current)))
    (append vector nil)))

(defun emacs-srs-trainer--sort-cards-by-queue (cards state now)
  "Return CARDS grouped by Anki-style study queue.

The queue buckets are ordered as Learning, To Review, then New.  Cards
inside each queue are shuffled when
`emacs-srs-trainer-shuffle-cards-within-queue' is non-nil."
  (let ((buckets nil))
    (dolist (card cards)
      (let* ((card-state (emacs-srs-trainer--state-for-card card state now))
             (type (emacs-srs-trainer-scheduler-card-type card-state))
             (bucket (assoc type buckets)))
        (if bucket
            (setcdr bucket (cons card (cdr bucket)))
          (push (cons type (list card)) buckets))))
    (cl-loop for type in '(learning review new)
             for bucket = (cdr (assoc type buckets))
             when bucket
             append (let ((ordered (nreverse bucket)))
                      (if emacs-srs-trainer-shuffle-cards-within-queue
                          (emacs-srs-trainer--shuffle-list ordered)
                        ordered)))))

(defun emacs-srs-trainer-due-cards (&optional all topic now state deck limit)
  "Return cards due for review.

When ALL is non-nil, return all cards.  When TOPIC is non-nil, only
return cards whose topic matches it.  DECK defaults to
`emacs-srs-trainer-default-deck'.  LIMIT caps the returned card count."
  (let* ((timestamp (or now (emacs-srs-trainer-scheduler-now)))
         (loaded-state (or state (emacs-srs-trainer-storage-load)))
         (cards (emacs-srs-trainer-deck-by-name
                 (or deck emacs-srs-trainer-default-deck))))
    (emacs-srs-trainer--limit-cards
     (cl-remove-if-not
      (lambda (card)
        (and (or (null topic)
                 (string= (emacs-srs-trainer-card-topic card) topic))
             (or all
                 (emacs-srs-trainer-scheduler-due-p
                  (emacs-srs-trainer--state-for-card card loaded-state timestamp)
                  timestamp))))
      (emacs-srs-trainer--sort-cards-by-queue cards loaded-state timestamp))
     limit)))

(defun emacs-srs-trainer--render-question
    (buffer card remaining state-counts due-counts card-type-label
            &optional practice limit)
  "Render CARD question in BUFFER with REMAINING and queue counts."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (emacs-srs-trainer-review-mode)
      (insert (format "Deck: %s\n" (plist-get card :deck)))
      (insert (format "State: %s\n" (emacs-srs-trainer-format-queue-counts
                                     state-counts)))
      (insert (format "Due now: %s\n" (emacs-srs-trainer-format-queue-counts
                                       due-counts)))
      (insert (format "Due: %d\n" remaining))
      (insert (format "Card type: %s\n\n" card-type-label))
      (when (emacs-srs-trainer--normalize-card-limit limit)
        (insert (format "Session limit: %s\n"
                        (emacs-srs-trainer--session-limit-message limit))))
      (when practice
        (insert "Practice mode: progress will not be changed.\n"))
      (when (or practice (emacs-srs-trainer--normalize-card-limit limit))
        (insert "\n"))
      (insert (format "Q: %s\n\n" (plist-get card :question)))
      (insert "Press the actual Emacs key sequence now.\n")
      (goto-char (point-min)))))

(defun emacs-srs-trainer--render-no-cards
    (buffer message &optional state-counts due-counts deck-name)
  "Render MESSAGE in BUFFER for an empty review."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (emacs-srs-trainer-review-mode)
      (insert (format "Deck: %s\n" (or deck-name emacs-srs-trainer-default-deck)))
      (insert (format "State: %s\n" (emacs-srs-trainer-format-queue-counts
                                     (or state-counts
                                         (emacs-srs-trainer--empty-queue-counts)))))
      (insert (format "Due now: %s\n" (emacs-srs-trainer-format-queue-counts
                                       (or due-counts
                                           (emacs-srs-trainer--empty-queue-counts)))))
      (insert "Due: 0\n\n")
      (insert message)
      (insert "\n")
      (goto-char (point-min)))))

(defun emacs-srs-trainer--render-complete
    (buffer deck-name reviewed correct-count state-counts due-counts
            &optional practice limit)
  "Render a completed review session in BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (due-total (or (plist-get due-counts :total) 0)))
      (erase-buffer)
      (emacs-srs-trainer-review-mode)
      (insert (format "Deck: %s\n" deck-name))
      (insert (format "State: %s\n"
                      (emacs-srs-trainer-format-queue-counts state-counts)))
      (insert (format "Due now: %s\n"
                      (emacs-srs-trainer-format-queue-counts due-counts)))
      (insert (format "Reviewed: %d\n" reviewed))
      (insert (format "Correct: %d\n" correct-count))
      (when (emacs-srs-trainer--normalize-card-limit limit)
        (insert (format "Session limit: %s\n"
                        (emacs-srs-trainer--session-limit-message limit))))
      (insert "\n")
      (cond
       (practice
        (insert "Practice complete. Review progress was not changed.\n"))
       ((= due-total 0)
        (insert "You have reviewed all due cards.\n"))
       (t
        (insert (format "Session complete. %d due card%s %s.\n"
                        due-total
                        (if (= due-total 1) "" "s")
                        (if (= due-total 1) "remains" "remain")))
        (insert "Increase the session limit or run review again to continue.\n")))
      (insert "\nm: main menu    q: quit\n")
      (goto-char (point-min)))))

(defun emacs-srs-trainer--reviewed-card-entries (&optional state now)
  "Return reviewed card entries sorted by most recent review first."
  (let* ((loaded-state (or state (emacs-srs-trainer-storage-load)))
         (timestamp (or now (emacs-srs-trainer-scheduler-now)))
         (entries nil))
    (dolist (card (emacs-srs-trainer-all-cards))
      (let* ((card-state (emacs-srs-trainer-storage-card-state
                          (emacs-srs-trainer-card-id card)
                          loaded-state
                          timestamp))
             (last-reviewed (plist-get card-state :last-reviewed)))
        (when last-reviewed
          (push (list :card card
                      :state card-state
                      :last-reviewed last-reviewed)
                entries))))
    (sort entries
          (lambda (a b)
            (> (plist-get a :last-reviewed)
               (plist-get b :last-reviewed))))))

(defun emacs-srs-trainer--format-result-word (result)
  "Return a display word for scheduler RESULT."
  (pcase result
    ('correct "Correct")
    ('incorrect "Redo")
    (_ "Unknown")))

(defun emacs-srs-trainer--insert-deck-button (deck-name state now)
  "Insert a welcome-screen button for DECK-NAME using STATE at NOW."
  (let* ((cards (emacs-srs-trainer-deck-by-name deck-name))
         (due-counts (emacs-srs-trainer-due-counts nil now state deck-name))
         (state-counts (emacs-srs-trainer-queue-counts nil now state nil deck-name))
         (due-total (or (plist-get due-counts :total) 0)))
    (insert-text-button
     deck-name
     'deck-name deck-name
     'follow-link t
     'help-echo "RET/mouse-1: review due cards in this deck"
     'action (lambda (button)
               (emacs-srs-trainer-review-deck
                (button-get button 'deck-name))))
    (insert (format "    Due: %d    Cards: %d    %s\n"
                    due-total
                    (length cards)
                    (emacs-srs-trainer-format-queue-counts state-counts)))))

(defun emacs-srs-trainer--render-welcome
    (buffer &optional reviewed correct-count state deck-name)
  "Render the welcome/dashboard screen in BUFFER."
  (let* ((loaded-state (or state (emacs-srs-trainer-storage-load)))
         (now (emacs-srs-trainer-scheduler-now))
         (recent (emacs-srs-trainer--reviewed-card-entries loaded-state now)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emacs-srs-trainer-welcome-mode)
        (insert "Emacs SRS Trainer\n")
        (insert "=================\n\n")
        (when reviewed
          (insert (format "Recently reviewed in this session: %d"
                          reviewed))
          (when correct-count
            (insert (format "    Correct: %d" correct-count)))
          (when deck-name
            (insert (format "    Deck: %s" deck-name)))
          (insert "\n\n"))
        (insert "Available decks\n")
        (insert "---------------\n")
        (dolist (name (emacs-srs-trainer-deck-names))
          (emacs-srs-trainer--insert-deck-button name loaded-state now))
        (insert "\nRecent reviews\n")
        (insert "--------------\n")
        (if recent
            (dolist (entry (cl-subseq recent 0 (min 10 (length recent))))
              (let* ((card (plist-get entry :card))
                     (card-state (plist-get entry :state))
                     (due (plist-get card-state :due))
                     (last-reviewed (plist-get entry :last-reviewed))
                     (result (plist-get card-state :last-result)))
                (insert (format "%s    %s    %s\n"
                                (emacs-srs-trainer--format-result-word result)
                                (emacs-srs-trainer-format-delay
                                 (- now last-reviewed))
                                (plist-get card :deck)))
                (insert (format "  %s\n" (plist-get card :question)))
                (when due
                  (insert (format "  Next due: %s\n"
                                  (if (<= due now)
                                      "now"
                                    (concat "in "
                                            (emacs-srs-trainer-format-delay
                                             (- due now)))))))
                (insert "\n")))
          (insert "No cards have been reviewed yet.\n"))
        (insert "\nTAB moves between deck buttons. RET opens the selected deck. ")
        (insert "Use ordinary Emacs window commands to leave this buffer.\n")
        (goto-char (point-min))))))

(defun emacs-srs-trainer--render-result
    (buffer card grade &optional card-state now practice)
  "Append GRADE result for CARD to BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "\nYou pressed: %s\n"
                      (emacs-srs-trainer-display-key
                       (plist-get grade :answer))))
      (if (plist-get grade :correct)
          (insert (propertize "Correct.\n"
                              'face 'emacs-srs-trainer-correct-face))
        (insert (propertize "Incorrect.\n"
                            'face 'emacs-srs-trainer-incorrect-face))
        (insert (format "Correct answer: %s\n"
                        (emacs-srs-trainer-card-display-answer card))))
      (if practice
          (insert "Practice mode: progress unchanged.\n")
        (when card-state
          (let* ((timestamp (or now (emacs-srs-trainer-scheduler-now)))
                 (type (emacs-srs-trainer-scheduler-card-type card-state))
                 (due (plist-get card-state :due)))
            (insert (format "Moved to: %s\n"
                            (emacs-srs-trainer-scheduler-card-type-label type)))
            (when due
              (let ((delay (if (<= due timestamp)
                               "now"
                             (concat "in "
                                     (emacs-srs-trainer-format-delay
                                      (- due timestamp))))))
                (insert (format "Next due: %s\n" delay)))))))
      (insert "\nRET or SPC: next    q: quit    ?: help\n")
      (goto-char (point-min)))))

(defun emacs-srs-trainer--render-help (buffer)
  "Append continuation help to BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\nAfter grading, RET and SPC continue; q quits the review. ")
      (insert "During answer capture, trainer controls are not active.\n"))))

(defun emacs-srs-trainer--review-cards
    (cards &optional buffer refresh-function topic deck practice limit)
  "Review CARDS in BUFFER and return a summary plist.

When REFRESH-FUNCTION is non-nil, call it after every answer with the
latest storage state and timestamp to refresh the due queue.  TOPIC and
DECK are used only for display counts.  When PRACTICE is non-nil, grade
cards without changing stored scheduler progress.  LIMIT caps the number
of cards reviewed in this session."
  (let* ((review-buffer (or buffer (get-buffer-create emacs-srs-trainer-review-buffer-name)))
         (deck-name (or deck emacs-srs-trainer-default-deck))
         (state (emacs-srs-trainer-storage-load))
         (session-limit (emacs-srs-trainer--normalize-card-limit limit))
         (cards (emacs-srs-trainer--limit-cards cards session-limit))
         (reviewed 0)
         (correct-count 0)
         (quit nil))
    (if (null cards)
        (progn
          (emacs-srs-trainer--render-no-cards
           review-buffer
           (if practice
               "No cards are available for practice."
             "You have reviewed all due cards. No cards are due right now.")
           (emacs-srs-trainer-queue-counts topic nil state nil deck-name)
           (emacs-srs-trainer-due-counts topic nil state deck-name)
           deck-name)
          (unless noninteractive (pop-to-buffer review-buffer))
          (list :reviewed 0 :correct 0 :quit nil
                :practice practice :limit session-limit))
      (unless noninteractive (pop-to-buffer review-buffer))
      (while (and cards
                  (not quit)
                  (or (null session-limit)
                      (< reviewed session-limit)))
        (let* ((card (car cards))
               (now (emacs-srs-trainer-scheduler-now))
               (state-counts (emacs-srs-trainer-queue-counts
                              topic now state nil deck-name))
               (due-counts (emacs-srs-trainer-due-counts
                            topic now state deck-name))
               (due-total (or (plist-get due-counts :total)
                              (length cards)))
               (unlimited-remaining (if refresh-function
                                        due-total
                                      (length cards)))
               (quota-left (and session-limit
                                (- session-limit reviewed)))
               (remaining (if quota-left
                              (min quota-left unlimited-remaining)
                            unlimited-remaining))
               (card-type-label (emacs-srs-trainer-card-type-label
                                 card state now)))
          (emacs-srs-trainer--render-question
           review-buffer card remaining state-counts due-counts card-type-label
           practice session-limit)
          (let* ((answer (let ((emacs-srs-trainer-current-card card))
                           (funcall emacs-srs-trainer-read-answer-function)))
                 (grade (emacs-srs-trainer-grade-answer card answer))
                 (correct (plist-get grade :correct)))
            (setq reviewed (1+ reviewed))
            (when correct
              (setq correct-count (1+ correct-count)))
            (let* ((result-now (emacs-srs-trainer-scheduler-now))
                   (card-state
                    (unless practice
                      (setq state (emacs-srs-trainer-storage-review-card
                                   (emacs-srs-trainer-card-id card)
                                   correct
                                   state))
                      (emacs-srs-trainer-storage-save state)
                      (emacs-srs-trainer-storage-card-state
                       (emacs-srs-trainer-card-id card)
                       state
                       result-now))))
              (emacs-srs-trainer--render-result
               review-buffer card grade card-state result-now practice))
            (let ((continuing t))
              (while continuing
                (setq emacs-srs-trainer--last-continuation-event nil)
                (pcase (funcall emacs-srs-trainer-read-continuation-function)
                  ('next
                   (emacs-srs-trainer--debounce-continuation-event
                    emacs-srs-trainer--last-continuation-event)
                   (setq continuing nil))
                  ('quit (setq quit t
                               continuing nil))
                  ('help (emacs-srs-trainer--render-help review-buffer))
                  (_ (setq continuing nil)))))))
        (setq cards (cdr cards))
        (when (and refresh-function
                   (not quit)
                   (or (null session-limit)
                       (< reviewed session-limit)))
          (setq cards (funcall refresh-function
                               state
                               (emacs-srs-trainer-scheduler-now)))))
      (cond
       (quit
        (emacs-srs-trainer--render-welcome
         review-buffer reviewed correct-count state deck-name))
       ((> reviewed 0)
        (let* ((complete-now (emacs-srs-trainer-scheduler-now))
               (state-counts (emacs-srs-trainer-queue-counts
                              topic complete-now state nil deck-name))
               (due-counts (emacs-srs-trainer-due-counts
                            topic complete-now state deck-name)))
          (emacs-srs-trainer--render-complete
           review-buffer deck-name reviewed correct-count
           state-counts due-counts practice session-limit)
          (when (and (not noninteractive)
                     (eq (funcall emacs-srs-trainer-read-completion-action-function)
                         'menu))
            (emacs-srs-trainer--render-welcome
             review-buffer reviewed correct-count state deck-name)))))
      (list :reviewed reviewed :correct correct-count :quit quit
            :practice practice :limit session-limit))))

;;;###autoload
(defun emacs-srs-trainer ()
  "Open the Emacs SRS Trainer welcome dashboard."
  (interactive)
  (let ((buffer (get-buffer-create emacs-srs-trainer-review-buffer-name)))
    (emacs-srs-trainer--render-welcome buffer)
    (unless noninteractive
      (pop-to-buffer buffer))
    buffer))

;;;###autoload
(defun emacs-srs-trainer-review (&optional limit)
  "Review due cards from the Emacs Tutorial deck.

LIMIT caps this session.  Interactively, a numeric prefix argument
sets LIMIT for one run.  Plain `C-u' prompts for a limit; 0 means no
limit.  Without a prefix, use `emacs-srs-trainer-review-card-limit'."
  (interactive
   (list (emacs-srs-trainer--read-card-limit-from-prefix
          current-prefix-arg)))
  (emacs-srs-trainer--review-cards
   (emacs-srs-trainer-due-cards nil nil nil nil nil limit)
   nil
   (lambda (state now)
     (emacs-srs-trainer-due-cards nil nil now state))
   nil
   nil
   nil
   limit))

;;;###autoload
(defun emacs-srs-trainer-review-all (&optional limit)
  "Practice all cards from the Emacs Tutorial deck, ignoring due dates.

This is a drill mode: answers are graded, but stored SRS progress is not
changed.  Interactively, a numeric prefix argument sets LIMIT for one
run.  Plain `C-u' prompts for a limit; 0 means no limit."
  (interactive
   (list (emacs-srs-trainer--read-card-limit-from-prefix
          current-prefix-arg t)))
  (emacs-srs-trainer--review-cards
   (emacs-srs-trainer-due-cards t nil nil nil nil limit)
   nil nil nil nil t limit))

;;;###autoload
(defun emacs-srs-trainer-review-deck (deck &optional prefix)
  "Review due cards from DECK.

With plain universal PREFIX, practice every card in DECK regardless of
due date without changing stored SRS progress.  With a numeric prefix,
limit the due review session to that many cards."
  (interactive
   (list (completing-read "Deck: "
                          (emacs-srs-trainer-deck-names)
                          nil t nil nil emacs-srs-trainer-default-deck)
         current-prefix-arg))
  (let* ((practice (emacs-srs-trainer--all-cards-prefix-p prefix))
         (limit (unless practice
                  (emacs-srs-trainer--read-card-limit-from-prefix prefix))))
    (emacs-srs-trainer--review-cards
     (emacs-srs-trainer-due-cards practice nil nil nil deck limit)
     nil
     (unless practice
       (lambda (state now)
         (emacs-srs-trainer-due-cards nil nil now state deck)))
     nil
     deck
     practice
     limit)))

;;;###autoload
(defun emacs-srs-trainer-review-topic (topic &optional prefix deck)
  "Review due cards for TOPIC in DECK.

With plain universal PREFIX, practice all cards in TOPIC regardless of
due date without changing stored SRS progress.  With a numeric prefix,
limit the due review session to that many cards.  DECK defaults to
`emacs-srs-trainer-default-deck' when this function is called from Lisp."
  (interactive
   (let* ((deck-name (completing-read "Deck: "
                                      (emacs-srs-trainer-deck-names)
                                      nil t nil nil
                                      emacs-srs-trainer-default-deck))
          (topics (emacs-srs-trainer-topics
                   (emacs-srs-trainer-deck-by-name deck-name))))
     (list (completing-read "Topic: " topics nil t)
           current-prefix-arg
           deck-name)))
  (let* ((deck-name (or deck emacs-srs-trainer-default-deck))
         (practice (emacs-srs-trainer--all-cards-prefix-p prefix))
         (limit (unless practice
                  (emacs-srs-trainer--read-card-limit-from-prefix prefix))))
    (emacs-srs-trainer--review-cards
     (emacs-srs-trainer-due-cards practice topic nil nil deck-name limit)
     nil
     (unless practice
       (lambda (state now)
         (emacs-srs-trainer-due-cards nil topic now state deck-name)))
     topic
     deck-name
     practice
     limit)))

;;;###autoload
(defun emacs-srs-trainer-set-review-card-limit (limit)
  "Set the default maximum number of due cards per review session.

Interactively, enter 0 for no limit.  The value is saved through
Customize so it persists across Emacs sessions."
  (interactive (list (emacs-srs-trainer--read-card-limit-value)))
  (if (called-interactively-p 'interactive)
      (customize-save-variable 'emacs-srs-trainer-review-card-limit limit)
    (setq emacs-srs-trainer-review-card-limit limit))
  (message "emacs-srs-trainer review limit: %s"
           (emacs-srs-trainer--session-limit-message limit)))

;;;###autoload
(defun emacs-srs-trainer-stats ()
  "Show review statistics."
  (interactive)
  (let* ((state (emacs-srs-trainer-storage-load))
         (cards (emacs-srs-trainer-deck-by-name emacs-srs-trainer-default-deck))
         (now (emacs-srs-trainer-scheduler-now))
         (due (emacs-srs-trainer-due-cards nil nil now state))
         (state-counts (emacs-srs-trainer-queue-counts nil now state))
         (due-counts (emacs-srs-trainer-due-counts nil now state))
         (reviewed (cl-count-if
                    (lambda (card)
                      (plist-get
                       (emacs-srs-trainer-storage-card-state
                        (emacs-srs-trainer-card-id card) state now)
                       :last-reviewed))
                    cards))
         (text (format (concat "Deck: %s\nCards: %d\nDue: %d\n"
                               "Review card limit: %s\n"
                               "State: %s\nDue now: %s\n"
                               "Reviewed: %d\nStorage: %s\n")
                       emacs-srs-trainer-default-deck
                       (length cards)
                       (length due)
                       (emacs-srs-trainer--session-limit-message
                        emacs-srs-trainer-review-card-limit)
                       (emacs-srs-trainer-format-queue-counts state-counts)
                       (emacs-srs-trainer-format-queue-counts due-counts)
                       reviewed
                       emacs-srs-trainer-storage-file)))
    (if noninteractive
        (princ text)
      (with-current-buffer (get-buffer-create "*Emacs SRS Trainer Stats*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (goto-char (point-min))
          (view-mode 1))
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun emacs-srs-trainer-reset (&optional force)
  "Reset all stored SRS progress.

Interactively, ask for confirmation unless FORCE is non-nil."
  (interactive "P")
  (when (or force
            noninteractive
            (yes-or-no-p "Reset all emacs-srs-trainer progress? "))
    (emacs-srs-trainer-storage-reset)
    (message "emacs-srs-trainer progress reset")))

;;;###autoload
(defun emacs-srs-trainer-open-deck ()
  "Open the built-in tutorial deck source."
  (interactive)
  (let ((file (or (locate-library "emacs-srs-trainer-deck")
                  (expand-file-name "emacs-srs-trainer-deck.el"
                                    (file-name-directory (or load-file-name
                                                             buffer-file-name
                                                             default-directory))))))
    (find-file file)))

(defun emacs-srs-trainer-doctor-report ()
  "Return a diagnostic report string."
  (let* ((cards (emacs-srs-trainer-deck-by-name emacs-srs-trainer-default-deck))
         (all-cards (emacs-srs-trainer-all-cards))
         (state (emacs-srs-trainer-storage-load))
         (now (emacs-srs-trainer-scheduler-now))
         (due (emacs-srs-trainer-due-cards nil nil now state))
         (state-counts (emacs-srs-trainer-queue-counts nil now state))
         (due-counts (emacs-srs-trainer-due-counts nil now state))
         (tutorial-summary (emacs-srs-trainer-tutorial-extraction-summary))
         (info-summary (emacs-srs-trainer-info-extraction-summary))
         (org-summary (emacs-srs-trainer-org-extraction-summary))
         (validation (emacs-srs-trainer-validate-deck-data))
         (storage-health (emacs-srs-trainer-storage-health))
         (decks (emacs-srs-trainer-load-decks)))
    (concat
     (format "emacs-srs-trainer doctor\n")
     (format "Version: %s\n" emacs-srs-trainer-version)
     (format "Storage location: %s\n" emacs-srs-trainer-storage-file)
     (format "Default deck: %s\n" emacs-srs-trainer-default-deck)
     (format "Review card limit: %s\n"
             (emacs-srs-trainer--session-limit-message
              emacs-srs-trainer-review-card-limit))
     (format "Loaded decks: %d\n" (length decks))
     (format "Cards: %d\n" (length all-cards))
     (format "Default deck cards: %d\n" (length cards))
     (format "Default deck due cards: %d\n" (length due))
     (format "State queues: %s\n"
             (emacs-srs-trainer-format-queue-counts state-counts))
     (format "Due now queues: %s\n"
             (emacs-srs-trainer-format-queue-counts due-counts))
     (format "Tutorial file: %s\n"
             (or (plist-get tutorial-summary :path) "not found"))
     (format "Tutorial extraction: %s (%d keys)\n"
             (if (plist-get tutorial-summary :ok) "ok" "failed")
             (or (plist-get tutorial-summary :count) 0))
     (format "Info manual file: %s\n"
             (or (plist-get info-summary :path) "not found"))
     (format "Info extraction: %s (%d keys)\n"
             (if (plist-get info-summary :ok) "ok" "failed")
             (or (plist-get info-summary :count) 0))
     (format "Org manual file: %s\n"
             (or (plist-get org-summary :path) "not found"))
     (format "Org extraction: %s (%d keys)\n"
             (if (plist-get org-summary :ok) "ok" "failed")
             (or (plist-get org-summary :count) 0))
     (format "Deck validation: %s\n"
             (if (plist-get validation :ok) "ok" "failed"))
     (format "Scheduler/storage health: %s (%s, %d stored card states)\n"
             (if (plist-get storage-health :ok) "ok" "failed")
             (plist-get storage-health :message)
             (or (plist-get storage-health :card-state-count) 0))
     (if (and (plist-get validation :ok)
              (plist-get tutorial-summary :ok)
              (plist-get info-summary :ok)
              (plist-get org-summary :ok)
              (plist-get storage-health :ok))
         "Overall: healthy\n"
       "Overall: needs attention\n")
     (when (plist-get validation :errors)
       (concat "\nValidation errors:\n"
               (mapconcat (lambda (error) (concat "- " error))
                          (plist-get validation :errors)
                          "\n")
               "\n")))))

;;;###autoload
(defun emacs-srs-trainer-doctor ()
  "Report package, deck, tutorial, scheduler, and storage health."
  (interactive)
  (let ((report (emacs-srs-trainer-doctor-report)))
    (if noninteractive
        (princ report)
      (with-current-buffer (get-buffer-create "*Emacs SRS Trainer Doctor*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report)
          (goto-char (point-min))
          (view-mode 1))
        (pop-to-buffer (current-buffer))))))

(provide 'emacs-srs-trainer)

;;; emacs-srs-trainer.el ends here
