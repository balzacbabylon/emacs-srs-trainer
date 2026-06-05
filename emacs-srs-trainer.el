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
(require 'emacs-srs-trainer-deck)
(require 'emacs-srs-trainer-scheduler)
(require 'emacs-srs-trainer-storage)
(require 'emacs-srs-trainer-tutorial)
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

(defvar emacs-srs-trainer-current-card nil
  "Card currently being answered by the review loop.")

(define-derived-mode emacs-srs-trainer-review-mode fundamental-mode "Emacs-SRS"
  "Major mode for the Emacs SRS review buffer."
  (setq-local cursor-type nil)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil))

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
            (setq events
                  (vconcat events
                           (vector (read-event "Answer key sequence: "))))
            (let ((description (emacs-srs-trainer-normalize-key events)))
              (cond
               ((member description normalized-answers)
                (setq done t))
               ((emacs-srs-trainer--answer-prefix-p
                 events answer-vectors normalized-answers)
                nil)
               (t
                (setq done t)))))
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
    (cond
     ((or (eq key ?\r) (eq key ?\s) (eq key ?\n)) 'next)
     ((eq key ?q) 'quit)
     ((eq key ??) 'help)
     (t 'next))))

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

(defun emacs-srs-trainer-queue-counts (&optional topic now state due-only)
  "Return queue counts for the default deck.

When TOPIC is non-nil, count only cards whose topic matches it.  When
DUE-ONLY is non-nil, count only cards due at NOW."
  (let* ((timestamp (or now (emacs-srs-trainer-scheduler-now)))
         (loaded-state (or state (emacs-srs-trainer-storage-load)))
         (cards (cl-remove-if-not
                 (lambda (card)
                   (or (null topic)
                       (string= (emacs-srs-trainer-card-topic card) topic)))
                 (emacs-srs-trainer-deck-by-name
                  emacs-srs-trainer-default-deck))))
    (emacs-srs-trainer--queue-counts-for-cards
     cards loaded-state timestamp due-only)))

(defun emacs-srs-trainer-due-counts (&optional topic now state)
  "Return due queue counts for the default deck.

When TOPIC is non-nil, count only cards whose topic matches it."
  (emacs-srs-trainer-queue-counts topic now state t))

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

(defun emacs-srs-trainer-due-cards (&optional all topic now state)
  "Return cards due for review.

When ALL is non-nil, return all cards.  When TOPIC is non-nil, only
return cards whose topic matches it."
  (let* ((timestamp (or now (emacs-srs-trainer-scheduler-now)))
         (loaded-state (or state (emacs-srs-trainer-storage-load)))
         (cards (emacs-srs-trainer-deck-by-name emacs-srs-trainer-default-deck)))
    (cl-remove-if-not
     (lambda (card)
       (and (or (null topic)
                (string= (emacs-srs-trainer-card-topic card) topic))
            (or all
                (emacs-srs-trainer-scheduler-due-p
                 (emacs-srs-trainer--state-for-card card loaded-state timestamp)
                 timestamp))))
     (emacs-srs-trainer--sort-cards-by-queue cards loaded-state timestamp))))

(defun emacs-srs-trainer--render-question
    (buffer card remaining state-counts due-counts card-type-label)
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
      (insert (format "Q: %s\n\n" (plist-get card :question)))
      (insert "Press the actual Emacs key sequence now.\n")
      (goto-char (point-min)))))

(defun emacs-srs-trainer--render-no-cards
    (buffer message &optional state-counts due-counts)
  "Render MESSAGE in BUFFER for an empty review."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (emacs-srs-trainer-review-mode)
      (insert (format "Deck: %s\n" emacs-srs-trainer-default-deck))
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

(defun emacs-srs-trainer--render-result
    (buffer card grade &optional card-state now)
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
      (when card-state
        (let* ((timestamp (or now (emacs-srs-trainer-scheduler-now)))
               (type (emacs-srs-trainer-scheduler-card-type card-state))
               (due (plist-get card-state :due)))
          (insert (format "Moved to: %s\n"
                          (emacs-srs-trainer-scheduler-card-type-label type)))
          (when due
            (insert (format "Next due: %s\n"
                            (if (<= due timestamp)
                                "now"
                              (concat "in "
                                      (emacs-srs-trainer-format-delay
                                       (- due timestamp)))))))))
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
    (cards &optional buffer refresh-function topic)
  "Review CARDS in BUFFER and return a summary plist.

When REFRESH-FUNCTION is non-nil, call it after every answer with the
latest storage state and timestamp to refresh the due queue.  TOPIC is
used only for display counts."
  (let* ((review-buffer (or buffer (get-buffer-create emacs-srs-trainer-review-buffer-name)))
         (state (emacs-srs-trainer-storage-load))
         (reviewed 0)
         (correct-count 0)
         (quit nil))
    (if (null cards)
        (progn
          (emacs-srs-trainer--render-no-cards
           review-buffer
           "No cards are due. Use M-x emacs-srs-trainer-review-all to drill anyway."
           (emacs-srs-trainer-queue-counts topic nil state)
           (emacs-srs-trainer-due-counts topic nil state))
          (unless noninteractive (pop-to-buffer review-buffer))
          (list :reviewed 0 :correct 0 :quit nil))
      (unless noninteractive (pop-to-buffer review-buffer))
      (while (and cards (not quit))
        (let* ((card (car cards))
               (now (emacs-srs-trainer-scheduler-now))
               (state-counts (emacs-srs-trainer-queue-counts
                              topic now state))
               (due-counts (emacs-srs-trainer-due-counts
                            topic now state))
               (remaining (if refresh-function
                              (or (plist-get due-counts :total)
                                  (length cards))
                            (length cards)))
               (card-type-label (emacs-srs-trainer-card-type-label
                                 card state now)))
          (emacs-srs-trainer--render-question
           review-buffer card remaining state-counts due-counts card-type-label)
          (let* ((answer (let ((emacs-srs-trainer-current-card card))
                           (funcall emacs-srs-trainer-read-answer-function)))
                 (grade (emacs-srs-trainer-grade-answer card answer))
                 (correct (plist-get grade :correct)))
            (setq reviewed (1+ reviewed))
            (when correct
              (setq correct-count (1+ correct-count)))
            (setq state (emacs-srs-trainer-storage-review-card
                         (emacs-srs-trainer-card-id card) correct state))
            (emacs-srs-trainer-storage-save state)
            (let* ((result-now (emacs-srs-trainer-scheduler-now))
                   (card-state (emacs-srs-trainer-storage-card-state
                                (emacs-srs-trainer-card-id card)
                                state
                                result-now)))
              (emacs-srs-trainer--render-result
               review-buffer card grade card-state result-now))
            (let ((continuing t))
              (while continuing
                (pcase (funcall emacs-srs-trainer-read-continuation-function)
                  ('next (setq continuing nil))
                  ('quit (setq quit t
                               continuing nil))
                  ('help (emacs-srs-trainer--render-help review-buffer))
                  (_ (setq continuing nil)))))))
        (setq cards (cdr cards))
        (when refresh-function
          (setq cards (funcall refresh-function
                               state
                               (emacs-srs-trainer-scheduler-now)))))
      (list :reviewed reviewed :correct correct-count :quit quit))))

;;;###autoload
(defun emacs-srs-trainer-review ()
  "Review due cards from the Emacs Tutorial deck."
  (interactive)
  (emacs-srs-trainer--review-cards
   (emacs-srs-trainer-due-cards)
   nil
   (lambda (state now)
     (emacs-srs-trainer-due-cards nil nil now state))))

;;;###autoload
(defun emacs-srs-trainer-review-all ()
  "Review all cards from the Emacs Tutorial deck, ignoring due dates."
  (interactive)
  (emacs-srs-trainer--review-cards (emacs-srs-trainer-due-cards t)))

;;;###autoload
(defun emacs-srs-trainer-review-topic (topic &optional all)
  "Review due cards for TOPIC.

With prefix argument ALL, review all cards in TOPIC regardless of
due date."
  (interactive
   (list (completing-read "Topic: " (emacs-srs-trainer-topics) nil t)
         current-prefix-arg))
  (emacs-srs-trainer--review-cards
   (emacs-srs-trainer-due-cards all topic)
   nil
   (unless all
     (lambda (state now)
       (emacs-srs-trainer-due-cards nil topic now state)))
   topic))

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
                               "State: %s\nDue now: %s\n"
                               "Reviewed: %d\nStorage: %s\n")
                       emacs-srs-trainer-default-deck
                       (length cards)
                       (length due)
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
         (state (emacs-srs-trainer-storage-load))
         (now (emacs-srs-trainer-scheduler-now))
         (due (emacs-srs-trainer-due-cards nil nil now state))
         (state-counts (emacs-srs-trainer-queue-counts nil now state))
         (due-counts (emacs-srs-trainer-due-counts nil now state))
         (tutorial-summary (emacs-srs-trainer-tutorial-extraction-summary))
         (validation (emacs-srs-trainer-validate-deck-data))
         (storage-health (emacs-srs-trainer-storage-health))
         (decks (emacs-srs-trainer-load-decks)))
    (concat
     (format "emacs-srs-trainer doctor\n")
     (format "Version: %s\n" emacs-srs-trainer-version)
     (format "Storage location: %s\n" emacs-srs-trainer-storage-file)
     (format "Loaded decks: %d\n" (length decks))
     (format "Cards: %d\n" (length cards))
     (format "Due cards: %d\n" (length due))
     (format "State queues: %s\n"
             (emacs-srs-trainer-format-queue-counts state-counts))
     (format "Due now queues: %s\n"
             (emacs-srs-trainer-format-queue-counts due-counts))
     (format "Tutorial file: %s\n"
             (or (plist-get tutorial-summary :path) "not found"))
     (format "Tutorial extraction: %s (%d keys)\n"
             (if (plist-get tutorial-summary :ok) "ok" "failed")
             (or (plist-get tutorial-summary :count) 0))
     (format "Deck validation: %s\n"
             (if (plist-get validation :ok) "ok" "failed"))
     (format "Scheduler/storage health: %s (%s, %d stored card states)\n"
             (if (plist-get storage-health :ok) "ok" "failed")
             (plist-get storage-health :message)
             (or (plist-get storage-health :card-state-count) 0))
     (if (and (plist-get validation :ok)
              (plist-get tutorial-summary :ok)
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
