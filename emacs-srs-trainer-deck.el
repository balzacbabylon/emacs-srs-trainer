;;; emacs-srs-trainer-deck.el --- Deck and grading helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: emacs-srs-trainer contributors
;; Maintainer: emacs-srs-trainer contributors
;; URL: https://github.com/balzacbabylon/emacs-srs-trainer
;; Keywords: learning, convenience
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Deck registration, key normalization, answer grading, and the
;; curated deck derived from the installed Emacs Tutorial.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emacs-srs-trainer nil
  "Emacs-native spaced repetition for Emacs keybindings."
  :group 'applications
  :prefix "emacs-srs-trainer-")

(defconst emacs-srs-trainer-version "0.1.0"
  "Version of `emacs-srs-trainer'.")

(defvar emacs-srs-trainer-decks nil
  "Registered decks as an alist of (DECK-NAME . CARDS).")

(defconst emacs-srs-trainer-reverse-card-id-suffix "::reverse"
  "Suffix used to store reverse-card scheduler state separately.")

(defconst emacs-srs-trainer-required-card-fields
  '(:id :deck :topic :question :canonical-answer :tags :source-ref)
  "Required plist keys for every card.")

(defconst emacs-srs-trainer-tutorial-deck-name "Emacs Tutorial"
  "Name of the built-in Emacs Tutorial deck.")

(defconst emacs-srs-trainer-info-deck-name "Info: An Introduction"
  "Name of the built-in Info introduction deck.")

(defconst emacs-srs-trainer-org-deck-name "Org Mode Manual"
  "Name of the built-in Org Mode manual deck.")

(defcustom emacs-srs-trainer-key-display-alist
  '(("DEL" . "Backspace/Delete (DEL)")
    ("M-DEL" . "M-Backspace/Delete (M-DEL)")
    ("<backspace>" . "Backspace/Delete (<backspace>)")
    ("M-<backspace>" . "M-Backspace/Delete (M-<backspace>)")
    ("<backtab>" . "Shift-TAB (<backtab>)"))
  "Alist mapping canonical key notation to friendlier display text."
  :type '(alist :key-type string :value-type string)
  :group 'emacs-srs-trainer)

(defun emacs-srs-trainer-deck--canonicalize-angle-token (token)
  "Canonicalize angle-bracket key names inside TOKEN.

`key-description' may report special keys with lowercase names during
capture, while manuals often spell them in uppercase.  Keep modifiers
unchanged and normalize the key name inside angle brackets."
  (if (string-match (rx bos (group (*? anything)) "<" (group (+ (not ">"))) ">" eos)
                    token)
      (let ((prefix (match-string 1 token))
            (name (match-string 2 token)))
        (pcase (downcase name)
          ("return" (concat prefix "RET"))
          ("tab" (concat prefix "TAB"))
          ("escape" (concat prefix "ESC"))
          ("backspace" (concat prefix "<backspace>"))
          (_ (concat prefix "<" (downcase name) ">"))))
    token))

(defun emacs-srs-trainer-deck--canonicalize-token (token)
  "Canonicalize one key-description TOKEN for comparison."
  (cond
   ((member token '("<return>" "<Return>")) "RET")
   ((member token '("<tab>" "<Tab>")) "TAB")
   ((member token '("<escape>" "<Escape>")) "ESC")
   ((member token '("<BACKSPACE>" "<Backspace>")) "<backspace>")
   ((string-match-p (rx "<" (+ (not ">")) ">" eos) token)
    (emacs-srs-trainer-deck--canonicalize-angle-token token))
   (t token)))

(defun emacs-srs-trainer-canonicalize-key-description (description)
  "Canonicalize key DESCRIPTION returned by `key-description'."
  (mapconcat #'emacs-srs-trainer-deck--canonicalize-token
             (split-string description " " t)
             " "))

(defun emacs-srs-trainer-deck--extended-command-token-p (token)
  "Return non-nil when TOKEN is part of an extended-command name."
  (and (not (member token '("RET" "SPC")))
       (string-match-p (rx bos (any alnum "-") eos) token)))

(defun emacs-srs-trainer-deck--normalize-extended-command-spaces
    (description)
  "Interpret spaces inside M-x command names as dashes in DESCRIPTION.

Emacs binds SPC in command-name completion to
`minibuffer-complete-word', and command completion treats spaces and
dashes as word delimiters.  Mirror that behavior for named-command
answers so typing \"replace string\" matches `replace-string'."
  (let ((tokens (split-string description " " t))
        (result nil)
        (reading-command nil)
        (seen-command-token nil))
    (dolist (token tokens)
      (cond
       ((string= token "M-x")
        (setq reading-command t
              seen-command-token nil)
        (push token result))
       ((and reading-command (string= token "RET"))
        (setq reading-command nil
              seen-command-token nil)
        (push token result))
       ((and reading-command
             seen-command-token
             (string= token "SPC"))
        (push "-" result))
       (t
        (when (and reading-command
                   (emacs-srs-trainer-deck--extended-command-token-p token))
          (setq seen-command-token t))
        (push token result))))
    (mapconcat #'identity (nreverse result) " ")))

(defun emacs-srs-trainer-normalize-key (key)
  "Return canonical Emacs notation for KEY.

KEY may be a string accepted by `kbd' or a vector returned by
`read-key-sequence-vector'."
  (emacs-srs-trainer-deck--normalize-extended-command-spaces
   (emacs-srs-trainer-canonicalize-key-description
    (cond
     ((vectorp key) (key-description key))
     ((stringp key) (key-description (kbd key)))
     (t (error "Unsupported key value: %S" key))))))

(defun emacs-srs-trainer-deck--compact-display-token-p (token)
  "Return non-nil when TOKEN is part of typed text for display."
  (string-match-p (rx bos (any alnum "-") eos) token))

(defun emacs-srs-trainer-deck--digit-display-token-p (token)
  "Return non-nil when TOKEN is one digit for display compaction."
  (string-match-p (rx bos digit eos) token))

(defun emacs-srs-trainer-deck--compact-display-tokens (tokens)
  "Compact prompt-like TOKENS for display.

Only text typed after `M-x' and consecutive digits after `C-u' are
compacted.  Ordinary keymap sequences keep their spaces, so `C-x 5 0'
and Org export dispatcher keys such as `C-c C-e l l' stay visually
faithful to Emacs and Org documentation."
  (let ((remaining tokens)
        (result nil))
    (while remaining
      (let ((token (pop remaining)))
        (cond
         ((string= token "M-x")
          (push token result)
          (let ((run nil))
            (while (and remaining
                        (not (string= (car remaining) "RET"))
                        (emacs-srs-trainer-deck--compact-display-token-p
                         (car remaining)))
              (push (pop remaining) run))
            (when run
              (push (mapconcat #'identity (nreverse run) "")
                    result))))
         ((string= token "C-u")
          (push token result)
          (let ((run nil))
            (while (and remaining
                        (emacs-srs-trainer-deck--digit-display-token-p
                         (car remaining)))
              (push (pop remaining) run))
            (when run
              (push (mapconcat #'identity (nreverse run) "")
                    result))))
         (t
          (push token result)))))
    (nreverse result)))

(defun emacs-srs-trainer-display-key-tokens (tokens)
  "Return user-facing display text for normalized key TOKENS."
  (mapconcat #'identity
             (emacs-srs-trainer-deck--compact-display-tokens tokens)
             " "))

(defun emacs-srs-trainer-display-key-description (description)
  "Return user-facing display text for normalized key DESCRIPTION."
  (emacs-srs-trainer-display-key-tokens
   (split-string description " " t)))

(defun emacs-srs-trainer-display-key (key)
  "Return a user-facing display string for KEY."
  (let ((normalized (emacs-srs-trainer-normalize-key key)))
    (or (cdr (assoc normalized emacs-srs-trainer-key-display-alist))
        (emacs-srs-trainer-display-key-description normalized))))

(defun emacs-srs-trainer-card-display-answer (card)
  "Return user-facing correct answer text for CARD."
  (or (plist-get card :display-answer)
      (emacs-srs-trainer-display-key (plist-get card :canonical-answer))))

(defun emacs-srs-trainer-card-answer-kind (card)
  "Return answer kind for CARD.

The result is `key' for key-sequence cards, `text' for active
definition-entry cards, or `passive' for self-graded cards."
  (or (plist-get card :answer-kind) 'key))

(defun emacs-srs-trainer-card-id (card)
  "Return CARD's id."
  (plist-get card :id))

(defun emacs-srs-trainer-card-topic (card)
  "Return CARD's topic."
  (plist-get card :topic))

(defun emacs-srs-trainer-card-answers (card)
  "Return CARD's canonical answer plus accepted alternatives."
  (delq nil
        (if (memq (emacs-srs-trainer-card-answer-kind card) '(text passive))
            (cons (plist-get card :text-answer)
                  (plist-get card :accepted-answers))
          (cons (plist-get card :canonical-answer)
                (plist-get card :accepted-answers)))))

(defun emacs-srs-trainer-normalize-text-answer (answer)
  "Return ANSWER normalized for active reverse-card comparison."
  (let ((text (downcase (or answer ""))))
    (setq text (replace-regexp-in-string (rx (one-or-more punct)) " " text))
    (setq text (replace-regexp-in-string (rx (one-or-more space)) " " text))
    (string-trim text)))

(defun emacs-srs-trainer-card-normalized-answers (card)
  "Return normalized accepted answer strings for CARD."
  (delete-dups
   (if (memq (emacs-srs-trainer-card-answer-kind card) '(text passive))
       (mapcar #'emacs-srs-trainer-normalize-text-answer
               (emacs-srs-trainer-card-answers card))
     (mapcar #'emacs-srs-trainer-normalize-key
             (emacs-srs-trainer-card-answers card)))))

(defun emacs-srs-trainer-grade-answer (card answer)
  "Grade ANSWER against CARD.

ANSWER may be a vector from `read-key-sequence-vector', key notation
string, or active text answer depending on CARD.  Return a plist with
:correct, :answer, and :accepted-answers."
  (let ((kind (emacs-srs-trainer-card-answer-kind card)))
    (if (memq kind '(text passive))
        (let* ((normalized-answer (emacs-srs-trainer-normalize-text-answer answer))
               (accepted (emacs-srs-trainer-card-normalized-answers card)))
          (list :correct (member normalized-answer accepted)
                :answer answer
                :normalized-answer normalized-answer
                :answer-kind kind
                :accepted-answers accepted))
      (let* ((normalized-answer (emacs-srs-trainer-normalize-key answer))
             (accepted (emacs-srs-trainer-card-normalized-answers card)))
        (list :correct (member normalized-answer accepted)
              :answer normalized-answer
              :answer-kind kind
              :accepted-answers accepted)))))

(defun emacs-srs-trainer-card-note-id (card)
  "Return the shared note id for CARD."
  (or (plist-get card :note-id)
      (emacs-srs-trainer-card-id card)))

(defun emacs-srs-trainer-reverse-card-id (card)
  "Return the reverse-card id for CARD."
  (concat (emacs-srs-trainer-card-note-id card)
          emacs-srs-trainer-reverse-card-id-suffix))

(defun emacs-srs-trainer-reverse-card (card &optional mode)
  "Return a reverse card view for CARD.

MODE is `passive' for self-graded definition recall or `active' for a
typed definition answer.  The reverse card shares CARD's note data but
uses its own id so scheduler progress is independent."
  (let ((answer-kind (if (eq mode 'active) 'text 'passive)))
    (list :id (emacs-srs-trainer-reverse-card-id card)
          :note-id (emacs-srs-trainer-card-note-id card)
          :card-template 'reverse
          :deck (plist-get card :deck)
          :topic (plist-get card :topic)
          :command (plist-get card :command)
          :question (emacs-srs-trainer-card-display-answer card)
          :canonical-answer (plist-get card :canonical-answer)
          :text-answer (plist-get card :question)
          :display-answer (plist-get card :question)
          :accepted-answers (plist-get card :accepted-definition-answers)
          :tags (plist-get card :tags)
          :source-ref (plist-get card :source-ref)
          :answer-kind answer-kind)))

(defun emacs-srs-trainer-register-deck (name cards)
  "Register CARDS under deck NAME."
  (let ((existing (assoc name emacs-srs-trainer-decks)))
    (if existing
        (setcdr existing cards)
      (push (cons name cards) emacs-srs-trainer-decks)))
  cards)

(defun emacs-srs-trainer-load-decks ()
  "Return all registered decks.

The built-in tutorial deck is registered when this file is loaded."
  emacs-srs-trainer-decks)

(defun emacs-srs-trainer-all-cards ()
  "Return all cards from all registered decks."
  (apply #'append (mapcar #'cdr (emacs-srs-trainer-load-decks))))

(defun emacs-srs-trainer-deck-by-name (name)
  "Return cards for deck NAME."
  (cdr (assoc name (emacs-srs-trainer-load-decks))))

(defun emacs-srs-trainer-deck-names ()
  "Return sorted names of registered decks."
  (sort (mapcar #'car (emacs-srs-trainer-load-decks)) #'string<))

(defun emacs-srs-trainer-topics (&optional cards)
  "Return sorted unique topics in CARDS, or all loaded cards."
  (sort (delete-dups (mapcar #'emacs-srs-trainer-card-topic
                             (or cards (emacs-srs-trainer-all-cards))))
        #'string<))

(defun emacs-srs-trainer-deck-permute-options (groups)
  "Return deterministic cartesian product of option GROUPS.

Each item in GROUPS is a list.  The result is a list of lists,
with group order and item order preserved."
  (if (null groups)
      (list nil)
    (cl-loop for item in (car groups)
             append (cl-loop for rest in (emacs-srs-trainer-deck-permute-options
                                          (cdr groups))
                             collect (cons item rest)))))

(defun emacs-srs-trainer-deck--card-for-deck
    (deck deck-tag id topic command question answer tags source-ref
          &optional accepted metadata)
  "Create one card plist for DECK with DECK-TAG."
  (list :id id
        :deck deck
        :topic topic
        :command command
        :question question
        :canonical-answer answer
        :accepted-answers accepted
        :tags (cons deck-tag tags)
        :source-ref source-ref
        :display-answer (plist-get metadata :display-answer)
        :metadata metadata))

(defun emacs-srs-trainer-deck--card
    (id topic command question answer tags source-ref &optional accepted metadata)
  "Create one tutorial deck card plist."
  (emacs-srs-trainer-deck--card-for-deck
   emacs-srs-trainer-tutorial-deck-name "tutorial"
   id topic command question answer tags source-ref accepted metadata))

(defun emacs-srs-trainer-deck--info-card
    (id topic command question answer tags source-ref &optional accepted metadata)
  "Create one Info introduction deck card plist."
  (emacs-srs-trainer-deck--card-for-deck
   emacs-srs-trainer-info-deck-name "info"
   id topic command question answer tags source-ref accepted metadata))

(defun emacs-srs-trainer-deck--org-card
    (id topic command question answer tags source-ref &optional accepted metadata)
  "Create one Org manual deck card plist."
  (emacs-srs-trainer-deck--card-for-deck
   emacs-srs-trainer-org-deck-name "org"
   id topic command question answer tags source-ref accepted metadata))

(defun emacs-srs-trainer-deck-generate-prefix-cards ()
  "Generate deterministic numeric-prefix cards for the tutorial deck."
  (let ((specs
         '(("forward-char" "Basic cursor movement" forward-char
            "Move forward 8 characters." "C-u 8 C-f" ("movement" "prefix")
            "TUTORIAL:Basic cursor control" ("M-8 C-f"))
           ("backward-char" "Basic cursor movement" backward-char
            "Move backward 8 characters." "C-u 8 C-b" ("movement" "prefix")
            "TUTORIAL:Basic cursor control" ("M-8 C-b"))
           ("next-line" "Basic cursor movement" next-line
            "Move down 8 lines." "C-u 8 C-n" ("movement" "prefix")
            "TUTORIAL:Basic cursor control" ("M-8 C-n"))
           ("previous-line" "Basic cursor movement" previous-line
            "Move up 8 lines." "C-u 8 C-p" ("movement" "prefix")
            "TUTORIAL:Basic cursor control" ("M-8 C-p"))
           ("scroll-up-lines" "Viewing" scroll-up-command
            "Scroll forward by 8 lines instead of one screen." "C-u 8 C-v"
            ("viewing" "prefix") "TUTORIAL:Basic cursor control" ("M-8 C-v"))
           ("scroll-down-lines" "Viewing" scroll-down-command
            "Scroll backward by 8 lines instead of one screen." "C-u 8 M-v"
            ("viewing" "prefix") "TUTORIAL:Basic cursor control" ("M-8 M-v"))
           ("insert-eight-stars" "Inserting and deleting" self-insert-command
            "Insert eight asterisks with one command." "C-u 8 *"
            ("insertion" "prefix") "TUTORIAL:Inserting and deleting" ("M-8 *"))
           ("kill-two-lines" "Inserting and deleting" kill-line
            "Kill two whole lines and their newlines." "C-u 2 C-k"
            ("killing" "prefix") "TUTORIAL:Inserting and deleting" ("M-2 C-k"))
           ("undo-three" "Undo" undo
            "Undo three changes with one undo command." "C-u 3 C-/"
            ("undo" "prefix") "TUTORIAL:Undo" ("M-3 C-/" "C-u 3 C-_" "C-u 3 C-x u"))
           ("recenter-top" "Viewing" recenter-top-bottom
            "Redisplay with point at the top using a zero prefix argument." "C-u 0 C-l"
            ("viewing" "prefix") "TUTORIAL:Windows" ("M-0 C-l"))
           ("set-fill-column-20" "Modes and filling" set-fill-column
            "Set the fill column to 20." "C-u 2 0 C-x f"
            ("filling" "prefix") "TUTORIAL:Modes and filling" ("M-2 M-0 C-x f")))))
    (cl-loop for spec in specs
             collect (apply #'emacs-srs-trainer-deck--card
                            (append (list (concat "tutorial-prefix-" (nth 0 spec))
                                          (nth 1 spec)
                                          (nth 2 spec)
                                          (nth 3 spec)
                                          (nth 4 spec)
                                          (nth 5 spec)
                                          (nth 6 spec)
                                          (nth 7 spec))
                                    (list (list :variation "numeric-prefix")))))))

(defconst emacs-srs-trainer-tutorial-base-cards
  (list
   ;; Orientation, quitting, and viewing.
   (emacs-srs-trainer-deck--card
    "tutorial-exit-emacs" "Orientation" 'save-buffers-kill-terminal
    "Exit Emacs, offering to save changed files first." "C-x C-c"
    '("exit") "TUTORIAL:Orientation")
   (emacs-srs-trainer-deck--card
    "tutorial-cancel-command" "Orientation" 'keyboard-quit
    "Quit or cancel a partially entered command." "C-g"
    '("quit") "TUTORIAL:Orientation")
   (emacs-srs-trainer-deck--card
    "tutorial-kill-tutorial-buffer" "Orientation" 'kill-buffer
    "Stop the tutorial by killing its buffer." "C-x k"
    '("buffers" "quit") "TUTORIAL:Orientation")
   (emacs-srs-trainer-deck--card
    "tutorial-finish-prompt" "Orientation" nil
    "Submit a minibuffer prompt or finish entering an argument." "RET"
    '("minibuffer") "TUTORIAL:Files" '("<Return>" "<return>"))
   (emacs-srs-trainer-deck--card
    "tutorial-scroll-forward" "Viewing" 'scroll-up-command
    "Move forward one screenful." "C-v"
    '("viewing") "TUTORIAL:Viewing")
   (emacs-srs-trainer-deck--card
    "tutorial-scroll-backward" "Viewing" 'scroll-down-command
    "Move backward one screenful." "M-v"
    '("viewing") "TUTORIAL:Viewing" '("<ESC> v"))
   (emacs-srs-trainer-deck--card
    "tutorial-recenter" "Viewing" 'recenter-top-bottom
    "Clear and redisplay the screen, cycling point through center, top, and bottom on repeated use." "C-l"
    '("viewing") "TUTORIAL:Viewing")

   ;; Basic cursor movement.
   (emacs-srs-trainer-deck--card
    "tutorial-move-forward-char" "Basic cursor movement" 'forward-char
    "Move forward one character." "C-f"
    '("movement") "TUTORIAL:Basic cursor control")
   (emacs-srs-trainer-deck--card
    "tutorial-move-backward-char" "Basic cursor movement" 'backward-char
    "Move backward one character." "C-b"
    '("movement") "TUTORIAL:Basic cursor control")
   (emacs-srs-trainer-deck--card
    "tutorial-next-line" "Basic cursor movement" 'next-line
    "Move to the next line." "C-n"
    '("movement") "TUTORIAL:Basic cursor control")
   (emacs-srs-trainer-deck--card
    "tutorial-previous-line" "Basic cursor movement" 'previous-line
    "Move to the previous line." "C-p"
    '("movement") "TUTORIAL:Basic cursor control")
   (emacs-srs-trainer-deck--card
    "tutorial-beginning-of-line" "Basic cursor movement" 'move-beginning-of-line
    "Move to the beginning of the line." "C-a"
    '("movement") "TUTORIAL:Basic cursor control")
   (emacs-srs-trainer-deck--card
    "tutorial-end-of-line" "Basic cursor movement" 'move-end-of-line
    "Move to the end of the line." "C-e"
    '("movement") "TUTORIAL:Basic cursor control")
   (emacs-srs-trainer-deck--card
    "tutorial-forward-word" "Basic cursor movement" 'forward-word
    "Move forward one word." "M-f"
    '("movement" "word") "TUTORIAL:Basic cursor control" '("<ESC> f"))
   (emacs-srs-trainer-deck--card
    "tutorial-backward-word" "Basic cursor movement" 'backward-word
    "Move backward one word." "M-b"
    '("movement" "word") "TUTORIAL:Basic cursor control" '("<ESC> b"))
   (emacs-srs-trainer-deck--card
    "tutorial-backward-sentence" "Basic cursor movement" 'backward-sentence
    "Move back to the beginning of the sentence." "M-a"
    '("movement" "sentence") "TUTORIAL:Basic cursor control" '("<ESC> a"))
   (emacs-srs-trainer-deck--card
    "tutorial-forward-sentence" "Basic cursor movement" 'forward-sentence
    "Move forward to the end of the sentence." "M-e"
    '("movement" "sentence") "TUTORIAL:Basic cursor control" '("<ESC> e"))
   (emacs-srs-trainer-deck--card
    "tutorial-beginning-of-buffer" "Basic cursor movement" 'beginning-of-buffer
    "Move to the beginning of the whole buffer." "M-<"
    '("movement" "buffer") "TUTORIAL:Basic cursor control" '("<ESC> <"))
   (emacs-srs-trainer-deck--card
    "tutorial-end-of-buffer" "Basic cursor movement" 'end-of-buffer
    "Move to the end of the whole buffer." "M->"
    '("movement" "buffer") "TUTORIAL:Basic cursor control" '("<ESC> >"))

   ;; Disabled commands and windows introduced early.
   (emacs-srs-trainer-deck--card
    "tutorial-disabled-downcase-region" "Disabled commands" 'downcase-region
    "Invoke the disabled command used by the tutorial as an example." "C-x C-l"
    '("disabled") "TUTORIAL:Disabled commands")
   (emacs-srs-trainer-deck--card
    "tutorial-one-window" "Windows" 'delete-other-windows
    "Delete all other windows and keep one window." "C-x 1"
    '("windows") "TUTORIAL:Windows")
   (emacs-srs-trainer-deck--card
    "tutorial-describe-key" "Help" 'describe-key
    "Display full documentation for the command run by a key sequence." "C-h k"
    '("help") "TUTORIAL:Windows")

   ;; Inserting and deleting.
   (emacs-srs-trainer-deck--card
    "tutorial-newline" "Inserting and deleting" 'newline
    "Insert a newline, with electric indentation when appropriate." "RET"
    '("insertion") "TUTORIAL:Inserting and deleting" '("<Return>" "<return>"))
   (emacs-srs-trainer-deck--card
    "tutorial-delete-backward-char" "Inserting and deleting" 'delete-backward-char
    "Remove the character just before point." "DEL"
    '("deletion") "TUTORIAL:Inserting and deleting"
    '("<DEL>" "<backspace>")
    '(:display-answer "Backspace/Delete (DEL)"))
   (emacs-srs-trainer-deck--card
    "tutorial-delete-forward-char" "Inserting and deleting" 'delete-char
    "Delete the next character after point." "C-d"
    '("deletion") "TUTORIAL:Inserting and deleting")
   (emacs-srs-trainer-deck--card
    "tutorial-backward-kill-word" "Inserting and deleting" 'backward-kill-word
    "Kill the word immediately before point." "M-DEL"
    '("killing" "word") "TUTORIAL:Inserting and deleting"
    '("M-<DEL>" "<ESC> DEL" "M-<backspace>" "<ESC> <backspace>")
    '(:display-answer "M-Backspace/Delete (M-DEL)"))
   (emacs-srs-trainer-deck--card
    "tutorial-kill-word" "Inserting and deleting" 'kill-word
    "Kill the next word after point." "M-d"
    '("killing" "word") "TUTORIAL:Inserting and deleting" '("<ESC> d"))
   (emacs-srs-trainer-deck--card
    "tutorial-kill-line" "Inserting and deleting" 'kill-line
    "Kill from point to the end of the line." "C-k"
    '("killing" "line") "TUTORIAL:Inserting and deleting")
   (emacs-srs-trainer-deck--card
    "tutorial-kill-sentence" "Inserting and deleting" 'kill-sentence
    "Kill to the end of the current sentence." "M-k"
    '("killing" "sentence") "TUTORIAL:Inserting and deleting" '("<ESC> k"))
   (emacs-srs-trainer-deck--card
    "tutorial-set-mark" "Inserting and deleting" 'set-mark-command
    "Set the mark before selecting a region to kill." "C-SPC"
    '("mark" "region") "TUTORIAL:Inserting and deleting" '("C-<SPC>"))
   (emacs-srs-trainer-deck--card
    "tutorial-kill-region" "Inserting and deleting" 'kill-region
    "Kill the active region." "C-w"
    '("killing" "region") "TUTORIAL:Inserting and deleting")
   (emacs-srs-trainer-deck--card
    "tutorial-yank" "Inserting and deleting" 'yank
    "Yank back the most recently killed text." "C-y"
    '("yank") "TUTORIAL:Inserting and deleting")
   (emacs-srs-trainer-deck--card
    "tutorial-yank-pop" "Inserting and deleting" 'yank-pop
    "After yanking, replace that text with an earlier kill." "M-y"
    '("yank") "TUTORIAL:Inserting and deleting" '("<ESC> y"))

   ;; Undo.
   (emacs-srs-trainer-deck--card
    "tutorial-undo" "Undo" 'undo
    "Undo the most recent change." "C-/"
    '("undo") "TUTORIAL:Undo")
   (emacs-srs-trainer-deck--card
    "tutorial-undo-control-underscore" "Undo" 'undo
    "Undo the most recent change with the tutorial's single-key alternate undo binding." "C-_"
    '("undo" "alternate") "TUTORIAL:Undo")
   (emacs-srs-trainer-deck--card
    "tutorial-undo-control-x" "Undo" 'undo
    "Undo the most recent change with the tutorial's multi-key alternate undo binding." "C-x u"
    '("undo" "alternate") "TUTORIAL:Undo")

   ;; Files and buffers.
   (emacs-srs-trainer-deck--card
    "tutorial-find-file" "Files" 'find-file
    "Find or visit a file." "C-x C-f"
    '("files") "TUTORIAL:Files")
   (emacs-srs-trainer-deck--card
    "tutorial-cancel-minibuffer" "Files" 'keyboard-quit
    "Cancel minibuffer input while finding a file." "C-g"
    '("files" "minibuffer" "quit") "TUTORIAL:Files")
   (emacs-srs-trainer-deck--card
    "tutorial-save-buffer" "Files" 'save-buffer
    "Save the current buffer to its file." "C-x C-s"
    '("files") "TUTORIAL:Files")
   (emacs-srs-trainer-deck--card
    "tutorial-list-buffers" "Buffers" 'list-buffers
    "List existing buffers." "C-x C-b"
    '("buffers") "TUTORIAL:Buffers")
   (emacs-srs-trainer-deck--card
    "tutorial-switch-buffer" "Buffers" 'switch-to-buffer
    "Switch to another buffer by name." "C-x b"
    '("buffers") "TUTORIAL:Buffers")
   (emacs-srs-trainer-deck--card
    "tutorial-save-some-buffers" "Files" 'save-some-buffers
    "Offer to save each modified file-visiting buffer." "C-x s"
    '("files" "buffers") "TUTORIAL:Buffers")

   ;; Extended commands, modes, filling, and packages.
   (emacs-srs-trainer-deck--card
    "tutorial-execute-extended-command" "Extended commands" 'execute-extended-command
    "Start a named extended command." "M-x"
    '("extended-command") "TUTORIAL:Extending the command set" '("<ESC> x"))
   (emacs-srs-trainer-deck--card
    "tutorial-suspend-emacs" "Extended commands" 'suspend-frame
    "Suspend Emacs temporarily from a text terminal." "C-z"
    '("exit") "TUTORIAL:Extending the command set")
   (emacs-srs-trainer-deck--card
    "tutorial-complete-command-name" "Extended commands" 'minibuffer-complete
    "Complete a command, file, or buffer name in the minibuffer." "TAB"
    '("completion" "minibuffer") "TUTORIAL:Extending the command set" '("<TAB>"))
   (emacs-srs-trainer-deck--card
    "tutorial-replace-string" "Extended commands" 'replace-string
    "Run the literal text replacement command by name." "M-x replace-string RET"
    '("extended-command" "replace") "TUTORIAL:Extending the command set"
    '("<ESC> x replace-string RET")
    '(:display-answer "M-x replace-string RET"))
   (emacs-srs-trainer-deck--card
    "tutorial-recover-this-file" "Files" 'recover-this-file
    "Recover the current file from its auto-save data by command name." "M-x recover-this-file RET"
    '("extended-command" "files") "TUTORIAL:Auto save"
    '("<ESC> x recover-this-file RET")
    '(:display-answer "M-x recover-this-file RET"))
   (emacs-srs-trainer-deck--card
    "tutorial-fundamental-mode" "Modes and filling" 'fundamental-mode
    "Switch to Fundamental mode by command name." "M-x fundamental-mode RET"
    '("extended-command" "modes") "TUTORIAL:Mode line"
    '("<ESC> x fundamental-mode RET")
    '(:display-answer "M-x fundamental-mode RET"))
   (emacs-srs-trainer-deck--card
    "tutorial-text-mode" "Modes and filling" 'text-mode
    "Switch to Text mode by command name." "M-x text-mode RET"
    '("extended-command" "modes") "TUTORIAL:Mode line"
    '("<ESC> x text-mode RET")
    '(:display-answer "M-x text-mode RET"))
   (emacs-srs-trainer-deck--card
    "tutorial-auto-fill-mode" "Modes and filling" 'auto-fill-mode
    "Toggle Auto Fill mode by command name." "M-x auto-fill-mode RET"
    '("extended-command" "modes" "filling") "TUTORIAL:Modes and filling"
    '("<ESC> x auto-fill-mode RET")
    '(:display-answer "M-x auto-fill-mode RET"))
   (emacs-srs-trainer-deck--card
    "tutorial-describe-mode" "Help" 'describe-mode
    "Describe the current major and minor modes." "C-h m"
    '("help" "modes") "TUTORIAL:Modes and filling")
   (emacs-srs-trainer-deck--card
    "tutorial-set-fill-column" "Modes and filling" 'set-fill-column
    "Set the fill column using a prefix argument." "C-x f"
    '("filling") "TUTORIAL:Modes and filling")
   (emacs-srs-trainer-deck--card
    "tutorial-fill-paragraph" "Modes and filling" 'fill-paragraph
    "Refill the paragraph at point." "M-q"
    '("filling") "TUTORIAL:Modes and filling" '("<ESC> q"))
   (emacs-srs-trainer-deck--card
    "tutorial-list-packages" "Packages" 'list-packages
    "Open the package menu listing installable packages by command name." "M-x list-packages RET"
    '("extended-command" "packages") "TUTORIAL:Installing packages"
    '("<ESC> x list-packages RET")
    '(:display-answer "M-x list-packages RET"))

   ;; Searching.
   (emacs-srs-trainer-deck--card
    "tutorial-isearch-forward" "Searching" 'isearch-forward
    "Start an incremental forward search." "C-s"
    '("search") "TUTORIAL:Searching")
   (emacs-srs-trainer-deck--card
    "tutorial-isearch-repeat-forward" "Searching" 'isearch-repeat-forward
    "During incremental search, move to the next occurrence." "C-s"
    '("search") "TUTORIAL:Searching")
   (emacs-srs-trainer-deck--card
    "tutorial-isearch-backward" "Searching" 'isearch-backward
    "Start an incremental reverse search." "C-r"
    '("search") "TUTORIAL:Searching")
   (emacs-srs-trainer-deck--card
    "tutorial-isearch-delete-char" "Searching" 'isearch-delete-char
    "During incremental search, retreat or erase the last search character." "DEL"
    '("search") "TUTORIAL:Searching"
    '("<DEL>" "<backspace>")
    '(:display-answer "Backspace/Delete (DEL)"))
   (emacs-srs-trainer-deck--card
    "tutorial-isearch-exit" "Searching" 'isearch-exit
    "Terminate an incremental search and keep point at the match." "RET"
    '("search") "TUTORIAL:Searching" '("<Return>" "<return>"))
   (emacs-srs-trainer-deck--card
    "tutorial-isearch-abort" "Searching" 'keyboard-quit
    "Terminate or abort an incremental search." "C-g"
    '("search" "quit") "TUTORIAL:Searching")

   ;; Windows and frames.
   (emacs-srs-trainer-deck--card
    "tutorial-split-window-below" "Windows" 'split-window-below
    "Split the selected window into two windows." "C-x 2"
    '("windows") "TUTORIAL:Multiple windows")
   (emacs-srs-trainer-deck--card
    "tutorial-scroll-other-window" "Windows" 'scroll-other-window
    "Scroll the other window while keeping this window selected." "C-M-v"
    '("windows" "viewing") "TUTORIAL:Multiple windows" '("<ESC> C-v"))
   (emacs-srs-trainer-deck--card
    "tutorial-other-window" "Windows" 'other-window
    "Move the cursor to the other window." "C-x o"
    '("windows") "TUTORIAL:Multiple windows")
   (emacs-srs-trainer-deck--card
    "tutorial-find-file-other-window" "Windows" 'find-file-other-window
    "Find a file in another window." "C-x 4 C-f"
    '("windows" "files") "TUTORIAL:Multiple windows")
   (emacs-srs-trainer-deck--card
    "tutorial-make-frame" "Frames" 'make-frame-command
    "Create a new graphical frame." "C-x 5 2"
    '("frames") "TUTORIAL:Multiple frames")
   (emacs-srs-trainer-deck--card
    "tutorial-delete-frame" "Frames" 'delete-frame
    "Remove the selected graphical frame." "C-x 5 0"
    '("frames") "TUTORIAL:Multiple frames")
   (emacs-srs-trainer-deck--card
    "tutorial-keyboard-escape-quit" "Recursive editing" 'keyboard-escape-quit
    "Leave a recursive edit, extra-window situation, or minibuffer with the tutorial's all-purpose multi-key quit command." "ESC ESC ESC"
    '("quit" "recursive-editing") "TUTORIAL:Recursive editing levels"
    '("<ESC> <ESC> <ESC>"))

   ;; Help and documentation.
   (emacs-srs-trainer-deck--card
    "tutorial-help-for-help" "Help" 'help-for-help
    "Show the list of available Help commands." "C-h ?"
    '("help") "TUTORIAL:Getting more help")
   (emacs-srs-trainer-deck--card
    "tutorial-describe-key-briefly" "Help" 'describe-key-briefly
    "Display a brief command name for a key sequence." "C-h c"
    '("help") "TUTORIAL:Getting more help")
   (emacs-srs-trainer-deck--card
    "tutorial-describe-command" "Help" 'describe-command
    "Describe a command by function name." "C-h x"
    '("help") "TUTORIAL:Getting more help")
   (emacs-srs-trainer-deck--card
    "tutorial-describe-variable" "Help" 'describe-variable
    "Describe a variable by name." "C-h v"
    '("help") "TUTORIAL:Getting more help")
   (emacs-srs-trainer-deck--card
    "tutorial-command-apropos" "Help" 'apropos-command
    "List commands whose names match a keyword." "C-h a"
    '("help") "TUTORIAL:Getting more help")
   (emacs-srs-trainer-deck--card
    "tutorial-info" "Help" 'info
    "Open the included manuals in Info." "C-h i"
    '("help" "info") "TUTORIAL:Getting more help")
   (emacs-srs-trainer-deck--card
    "tutorial-emacs-manual" "Help" 'info-emacs-manual
    "Open the Emacs manual directly." "C-h r"
    '("help" "manual") "TUTORIAL:More features")))

(defconst emacs-srs-trainer-tutorial-cards
  (append emacs-srs-trainer-tutorial-base-cards
          (emacs-srs-trainer-deck-generate-prefix-cards))
  "Curated Emacs Tutorial deck.")

(defconst emacs-srs-trainer-info-introduction-cards
  (list
   ;; Top-level orientation and basic node movement.
   (emacs-srs-trainer-deck--info-card
    "info-start-instruction-sequence" "Orientation" 'Info-help
    "Start the programmed instruction sequence for using Info." "h"
    '("orientation" "help") "INFO:Top")
   (emacs-srs-trainer-deck--info-card
    "info-command-summary" "Orientation" 'Info-summary
    "Show the Info command summary." "?"
    '("orientation" "help") "INFO:Top")
   (emacs-srs-trainer-deck--info-card
    "info-open-info-reader" "Orientation" 'info
    "Open the Info manual reader from Emacs Help." "C-h i"
    '("orientation" "help") "INFO:Create Info buffer")
   (emacs-srs-trainer-deck--info-card
    "info-next-node" "Node movement" 'Info-next
    "Go to the following node at the same level." "n"
    '("movement" "nodes") "INFO:Help")
   (emacs-srs-trainer-deck--info-card
    "info-previous-node" "Node movement" 'Info-prev
    "Go to the previous node at the same level." "p"
    '("movement" "nodes") "INFO:Help-P")
   (emacs-srs-trainer-deck--info-card
    "info-scroll-forward" "Node movement" 'Info-scroll-up
    "Scroll forward one screenful in Info." "SPC"
    '("movement" "scrolling") "INFO:Help-Small-Screen")
   (emacs-srs-trainer-deck--info-card
    "info-scroll-backward" "Node movement" 'Info-scroll-down
    "Scroll backward one screenful in Info." "DEL"
    '("movement" "scrolling") "INFO:Help-Small-Screen"
    '("<backspace>" "<BACKSPACE>" "S-SPC" "S-<SPC>")
    '(:display-answer "Backspace/Delete (DEL)"))
   (emacs-srs-trainer-deck--info-card
    "info-beginning-of-node" "Node movement" 'beginning-of-buffer
    "Move to the beginning of the current Info node." "b"
    '("movement" "nodes") "INFO:Help-^L")
   (emacs-srs-trainer-deck--info-card
    "info-recenter-display" "Node movement" 'recenter-top-bottom
    "Redisplay an Info screen that looks garbled." "C-l"
    '("movement" "display") "INFO:Help-^L")
   (emacs-srs-trainer-deck--info-card
    "info-next-preorder-node" "Node movement" 'Info-forward-node
    "Move immediately to the following node in file order regardless of tree level." "]"
    '("movement" "nodes") "INFO:Help-]")
   (emacs-srs-trainer-deck--info-card
    "info-previous-preorder-node" "Node movement" 'Info-backward-node
    "Move immediately to the preceding node in file order regardless of tree level." "["
    '("movement" "nodes") "INFO:Help-]")
   (emacs-srs-trainer-deck--info-card
    "info-visible-mode" "Emacs Info display" 'visible-mode
    "Toggle visibility of hidden Info link text by command name." "M-x visible-mode RET"
    '("display" "extended-command") "INFO:Help-Inv"
    '("<ESC> x visible-mode RET")
    '(:display-answer "M-x visible-mode RET"))

   ;; Menus, links, references, and history.
   (emacs-srs-trainer-deck--info-card
    "info-menu-by-name" "Menus and links" 'Info-menu
    "Start choosing a menu subtopic by name." "m"
    '("menus") "INFO:Help-M")
   (emacs-srs-trainer-deck--info-card
    "info-cancel-prompt" "Menus and links" 'keyboard-quit
    "Cancel an active menu, reference, or node-name prompt." "C-g"
    '("menus" "references" "quit") "INFO:Help-M")
   (emacs-srs-trainer-deck--info-card
    "info-complete-menu-or-reference-name" "Menus and links" 'minibuffer-complete
    "Complete a partially typed menu item or reference name." "TAB"
    '("menus" "references" "completion") "INFO:Help-M")
   (emacs-srs-trainer-deck--info-card
    "info-next-link" "Menus and links" 'Info-next-reference
    "Move point to the next menu item or cross-reference link." "TAB"
    '("menus" "references" "movement") "INFO:Help-M")
   (emacs-srs-trainer-deck--info-card
    "info-previous-link" "Menus and links" 'Info-prev-reference
    "Move point to the previous menu item or cross-reference link." "<backtab>"
    '("menus" "references" "movement") "INFO:Help-M"
    '("M-TAB" "C-M-i" "S-<tab>" "S-TAB")
    '(:display-answer "Shift-TAB (<backtab>)"))
   (emacs-srs-trainer-deck--info-card
    "info-follow-nearest-link" "Menus and links" 'Info-follow-nearest-node
    "Follow the link at point or accept the default menu/reference target." "RET"
    '("menus" "references") "INFO:Help-M" '("<Return>" "<return>"))
   (emacs-srs-trainer-deck--info-card
    "info-up-node" "Menus and links" 'Info-up
    "Move from a subnode to its parent node." "u"
    '("movement" "nodes") "INFO:Help-FOO")
   (emacs-srs-trainer-deck--info-card
    "info-follow-reference-by-name" "Cross references" 'Info-follow-reference
    "Start choosing a cross-reference by name." "f"
    '("references") "INFO:Help-Xref")
   (emacs-srs-trainer-deck--info-card
    "info-list-reference-names" "Cross references" 'Info-follow-reference
    "While choosing a cross-reference by name display the available reference names." "f ?"
    '("references" "completion") "INFO:Help-Xref")
   (emacs-srs-trainer-deck--info-card
    "info-history-back" "History" 'Info-history-back
    "Retrace one step backward through Info history." "l"
    '("history" "movement") "INFO:Help-Int")
   (emacs-srs-trainer-deck--info-card
    "info-history-forward" "History" 'Info-history-forward
    "Move forward through Info history after retracing." "r"
    '("history" "movement") "INFO:Help-Int")
   (emacs-srs-trainer-deck--info-card
    "info-history-list" "History" 'Info-history
    "Display a virtual node listing visited Info nodes." "L"
    '("history") "INFO:Help-Int")
   (emacs-srs-trainer-deck--info-card
    "info-directory" "Navigation shortcuts" 'Info-directory
    "Jump to the Info directory node." "d"
    '("movement" "directory") "INFO:Help-Int")
   (emacs-srs-trainer-deck--info-card
    "info-top-node" "Navigation shortcuts" 'Info-top-node
    "Jump to the top node of the current manual." "t"
    '("movement" "nodes") "INFO:Help-Int")
   (emacs-srs-trainer-deck--info-card
    "info-quit" "Orientation" 'quit-window
    "Quit Info and return to the previous Emacs window setup." "q"
    '("quit") "INFO:Help-Q")

   ;; Advanced Info commands.
   (emacs-srs-trainer-deck--info-card
    "info-quoted-insert" "Advanced search" 'quoted-insert
    "Quote the next input character so it is inserted literally in an Info prompt." "C-q"
    '("advanced" "search" "quoting") "INFO:Advanced")
   (emacs-srs-trainer-deck--info-card
    "info-quoted-insert-question-mark" "Advanced search" 'quoted-insert
    "Insert a literal question-mark character into an Info prompt such as a search string." "C-q ?"
    '("advanced" "search" "quoting") "INFO:Advanced")
   (emacs-srs-trainer-deck--info-card
    "info-search-text" "Advanced search" 'Info-search
    "Search the text of the current Info file for a string." "s"
    '("advanced" "search") "INFO:Search Text")
   (emacs-srs-trainer-deck--info-card
    "info-isearch-forward" "Advanced search" 'isearch-forward
    "Start an incremental forward search in Info." "C-s"
    '("advanced" "search" "isearch") "INFO:Search Text")
   (emacs-srs-trainer-deck--info-card
    "info-isearch-backward" "Advanced search" 'isearch-backward
    "Start an incremental reverse search in Info." "C-r"
    '("advanced" "search" "isearch") "INFO:Search Text")
   (emacs-srs-trainer-deck--info-card
    "info-index-search" "Advanced index search" 'Info-index
    "Look up a subject in the manual's indices." "i"
    '("advanced" "index") "INFO:Search Index")
   (emacs-srs-trainer-deck--info-card
    "info-index-next" "Advanced index search" 'Info-index-next
    "Move to the next matching index entry after an index lookup." ","
    '("advanced" "index") "INFO:Search Index")
   (emacs-srs-trainer-deck--info-card
    "info-virtual-index" "Advanced index search" 'Info-virtual-index
    "Build a virtual node showing index search results." "I"
    '("advanced" "index") "INFO:Search Index")
   (emacs-srs-trainer-deck--info-card
    "info-apropos" "Advanced index search" 'info-apropos
    "Search all installed Info indices by command name." "M-x info-apropos RET"
    '("advanced" "index" "extended-command") "INFO:Search Index"
    '("<ESC> x info-apropos RET")
    '(:display-answer "M-x info-apropos RET"))
   (emacs-srs-trainer-deck--info-card
    "info-goto-node" "Advanced node jumps" 'Info-goto-node
    "Start a jump to an Info node by name." "g"
    '("advanced" "movement" "nodes") "INFO:Go to node")
   (emacs-srs-trainer-deck--info-card
    "info-goto-whole-file" "Advanced node jumps" 'Info-goto-node
    "Display every node in the current Info file as one whole-file view." "g * RET"
    '("advanced" "movement" "nodes") "INFO:Go to node" '("g * <Return>" "g * <return>"))
   (emacs-srs-trainer-deck--info-card
    "info-first-menu-item" "Advanced menu shortcuts" 'Info-nth-menu-item
    "Choose the first menu item by number." "1"
    '("advanced" "menus") "INFO:Choose menu subtopic")
   (emacs-srs-trainer-deck--info-card
    "info-ninth-menu-item" "Advanced menu shortcuts" 'Info-nth-menu-item
    "Choose the ninth menu item by number." "9"
    '("advanced" "menus") "INFO:Choose menu subtopic")
   (emacs-srs-trainer-deck--info-card
    "info-clone-buffer" "Advanced Info buffers" 'clone-buffer
    "Create an independent copy of the current Info buffer in another window." "M-n"
    '("advanced" "buffers" "windows") "INFO:Create Info buffer" '("<ESC> n"))
   (emacs-srs-trainer-deck--info-card
    "info-menu-new-buffer" "Advanced Info buffers" 'Info-menu
    "Open a selected menu item in a new Info buffer." "C-u m"
    '("advanced" "buffers" "menus" "prefix") "INFO:Create Info buffer")
   (emacs-srs-trainer-deck--info-card
    "info-goto-new-buffer" "Advanced Info buffers" 'Info-goto-node
    "Open a named node in a new Info buffer." "C-u g"
    '("advanced" "buffers" "nodes" "prefix") "INFO:Create Info buffer")
   (emacs-srs-trainer-deck--info-card
    "info-numbered-info-buffer" "Advanced Info buffers" 'info
    "Switch to the second numbered Info buffer creating it if needed." "C-u 2 C-h i"
    '("advanced" "buffers" "prefix") "INFO:Create Info buffer")
   (emacs-srs-trainer-deck--info-card
    "info-display-manual" "Advanced Info buffers" 'info-display-manual
    "Show a manual by name while reusing an existing Info buffer when possible." "M-x info-display-manual RET"
    '("advanced" "buffers" "extended-command") "INFO:Create Info buffer"
    '("<ESC> x info-display-manual RET")
    '(:display-answer "M-x info-display-manual RET")))
  "Curated deck derived from the installed Info manual introduction.")

(defconst emacs-srs-trainer-org-manual-card-specs
  '(("org-cycle-visibility" "Document structure" org-cycle
     "Cycle visibility for the current Org item." "TAB"
     ("outline" "visibility") "ORG:Global and local cycling" ("<tab>"))
    ("org-global-cycle" "Document structure" org-global-cycle
     "Cycle visibility for the whole Org buffer." "S-TAB"
     ("outline" "visibility") "ORG:Global and local cycling" ("<backtab>"))
    ("org-cycle-children" "Document structure" org-cycle
     "Show one level of children for the current subtree." "C-u TAB"
     ("outline" "visibility" "prefix") "ORG:Global and local cycling" ("C-u <tab>"))
    ("org-set-startup-visibility" "Document structure" org-set-startup-visibility
     "Switch the buffer to the startup visibility state." "C-u C-u TAB"
     ("outline" "visibility" "prefix") "ORG:Global and local cycling" ("C-u C-u <tab>"))
    ("org-show-all" "Document structure" org-show-all
     "Expose every heading and body in the Org buffer." "C-u C-u C-u TAB"
     ("outline" "visibility" "prefix") "ORG:Global and local cycling" ("C-u C-u C-u <tab>"))
    ("org-reveal" "Document structure" org-reveal
     "Reveal the context around point inside hidden outline text." "C-c C-r"
     ("outline" "visibility") "ORG:Global and local cycling")
    ("org-show-branches" "Document structure" org-show-branches
     "Show all headings while keeping body text hidden." "C-c C-k"
     ("outline" "visibility") "ORG:Global and local cycling")
    ("org-show-children" "Document structure" org-show-children
     "Show the direct children of the current heading." "C-c <TAB>"
     ("outline" "visibility") "ORG:Global and local cycling" ("C-c TAB"))
    ("org-tree-to-indirect-buffer" "Document structure" org-tree-to-indirect-buffer
     "Narrow an indirect buffer to the current subtree." "C-c C-x b"
     ("outline" "narrowing" "buffers") "ORG:Global and local cycling")
    ("org-copy-visible" "Document structure" org-copy-visible
     "Copy only the visible text in the current region." "C-c C-x v"
     ("outline" "visibility" "copy") "ORG:Global and local cycling")
    ("org-next-visible-heading" "Motion" org-next-visible-heading
     "Move to the next visible heading." "C-c C-n"
     ("outline" "motion") "ORG:Motion")
    ("org-previous-visible-heading" "Motion" org-previous-visible-heading
     "Move to the previous visible heading." "C-c C-p"
     ("outline" "motion") "ORG:Motion")
    ("org-forward-heading-same-level" "Motion" org-forward-heading-same-level
     "Move to the next heading at the same level." "C-c C-f"
     ("outline" "motion") "ORG:Motion")
    ("org-backward-heading-same-level" "Motion" org-backward-heading-same-level
     "Move to the previous heading at the same level." "C-c C-b"
     ("outline" "motion") "ORG:Motion")
    ("org-up-heading" "Motion" outline-up-heading
     "Move from a heading to its parent heading." "C-c C-u"
     ("outline" "motion") "ORG:Motion")
    ("org-goto" "Motion" org-goto
     "Jump to another heading using Org's outline-location interface." "C-c C-j"
     ("outline" "motion") "ORG:Motion")
    ("org-meta-return" "Structure editing" org-meta-return
     "Insert a new heading, item, or row appropriate to the current Org context." "M-RET"
     ("outline" "editing") "ORG:Structure Editing" ("<ESC> RET"))
    ("org-insert-heading-respect-content" "Structure editing" org-insert-heading-respect-content
     "Insert a new heading after the current subtree." "C-RET"
     ("outline" "editing") "ORG:Structure Editing")
    ("org-insert-todo-heading" "Structure editing" org-insert-todo-heading
     "Insert a new TODO heading at the current level." "M-S-RET"
     ("outline" "todo" "editing") "ORG:Structure Editing")
    ("org-insert-todo-heading-respect-content" "Structure editing" org-insert-todo-heading-respect-content
     "Insert a new TODO heading after the current subtree." "C-S-RET"
     ("outline" "todo" "editing") "ORG:Structure Editing")
    ("org-do-promote" "Structure editing" org-do-promote
     "Promote the current heading by one level." "M-<LEFT>"
     ("outline" "editing") "ORG:Structure Editing")
    ("org-do-demote" "Structure editing" org-do-demote
     "Demote the current heading by one level." "M-<RIGHT>"
     ("outline" "editing") "ORG:Structure Editing")
    ("org-promote-subtree" "Structure editing" org-promote-subtree
     "Promote the entire current subtree by one level." "M-S-<LEFT>"
     ("outline" "editing") "ORG:Structure Editing")
    ("org-demote-subtree" "Structure editing" org-demote-subtree
     "Demote the entire current subtree by one level." "M-S-<RIGHT>"
     ("outline" "editing") "ORG:Structure Editing")
    ("org-move-subtree-up" "Structure editing" org-move-subtree-up
     "Move the current subtree above its previous sibling." "M-<UP>"
     ("outline" "editing") "ORG:Structure Editing")
    ("org-move-subtree-down" "Structure editing" org-move-subtree-down
     "Move the current subtree below its next sibling." "M-<DOWN>"
     ("outline" "editing") "ORG:Structure Editing")
    ("org-cut-subtree" "Structure editing" org-cut-subtree
     "Kill the current subtree as a structural unit." "C-c C-x C-w"
     ("outline" "editing" "killing") "ORG:Structure Editing")
    ("org-copy-subtree" "Structure editing" org-copy-subtree
     "Copy the current subtree as a structural unit." "C-c C-x M-w"
     ("outline" "editing" "copy") "ORG:Structure Editing")
    ("org-paste-subtree" "Structure editing" org-paste-subtree
     "Yank a subtree into the outline." "C-c C-x C-y"
     ("outline" "editing" "yank") "ORG:Structure Editing")
    ("org-clone-subtree-with-time-shift" "Structure editing" org-clone-subtree-with-time-shift
     "Clone the current subtree and shift its dates." "C-c C-x c"
     ("outline" "editing" "dates") "ORG:Structure Editing")
    ("org-mark-subtree" "Structure editing" org-mark-subtree
     "Select the current subtree." "C-c @"
     ("outline" "region") "ORG:Structure Editing")
    ("org-narrow-to-subtree" "Structure editing" org-narrow-to-subtree
     "Restrict the buffer view to the current subtree." "C-x n s"
     ("outline" "narrowing") "ORG:Structure Editing")
    ("org-narrow-to-block" "Structure editing" org-narrow-to-block
     "Restrict the buffer view to the current block." "C-x n b"
     ("blocks" "narrowing") "ORG:Structure Editing")
    ("widen" "Structure editing" widen
     "Remove narrowing and show the whole buffer again." "C-x n w"
     ("outline" "narrowing") "ORG:Structure Editing")
    ("org-sparse-tree" "Sparse trees" org-sparse-tree
     "Open the dispatcher for building a sparse tree." "C-c /"
     ("outline" "search") "ORG:Sparse Trees")
    ("org-occur" "Sparse trees" org-occur
     "Build a sparse tree from a regular-expression search." "C-c / r"
     ("outline" "search") "ORG:Sparse Trees")
    ("org-tags-sparse-tree" "Sparse trees" org-tags-sparse-tree
     "Build a sparse tree from a tag match." "C-c / m"
     ("outline" "tags" "search") "ORG:Tag Searches")
    ("org-todo-tree" "Sparse trees" org-show-todo-tree
     "Build a sparse tree of TODO entries." "C-c / t"
     ("outline" "todo" "search") "ORG:TODO Basics")
    ("org-agenda-check-deadlines" "Sparse trees" org-check-deadlines
     "Build a sparse tree of upcoming deadlines." "C-c / d"
     ("outline" "agenda" "dates") "ORG:Inserting deadline/schedule")
    ("org-agenda-check-before-date" "Sparse trees" org-check-before-date
     "Build a sparse tree for timestamps before a date." "C-c / b"
     ("outline" "agenda" "dates") "ORG:Inserting deadline/schedule")
    ("org-agenda-check-after-date" "Sparse trees" org-check-after-date
     "Build a sparse tree for timestamps after a date." "C-c / a"
     ("outline" "agenda" "dates") "ORG:Inserting deadline/schedule")
    ("org-table-align" "Tables" org-table-align
     "Realign the current Org grid." "C-c C-c"
     ("tables") "ORG:Built-in Table Editor")
    ("org-table-next-field" "Tables" org-table-next-field
     "Move to the next field in an Org grid." "TAB"
     ("tables" "motion") "ORG:Built-in Table Editor" ("<tab>"))
    ("org-table-previous-field" "Tables" org-table-previous-field
     "Move to the previous field in an Org grid." "S-TAB"
     ("tables" "motion") "ORG:Built-in Table Editor" ("<backtab>"))
    ("org-table-next-row" "Tables" org-table-next-row
     "Move to the next row in an Org grid." "RET"
     ("tables" "motion") "ORG:Built-in Table Editor" ("<return>"))
    ("org-table-beginning-of-field" "Tables" org-table-beginning-of-field
     "Move to the beginning of the current Org grid field." "M-a"
     ("tables" "motion") "ORG:Built-in Table Editor")
    ("org-table-end-of-field" "Tables" org-table-end-of-field
     "Move to the end of the current Org grid field." "M-e"
     ("tables" "motion") "ORG:Built-in Table Editor")
    ("org-table-move-column-left" "Tables" org-table-move-column-left
     "Move the current Org grid column left." "M-<LEFT>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-move-column-right" "Tables" org-table-move-column-right
     "Move the current Org grid column right." "M-<RIGHT>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-delete-column" "Tables" org-table-delete-column
     "Delete the current Org grid column." "M-S-<LEFT>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-insert-column" "Tables" org-table-insert-column
     "Insert a new Org grid column." "M-S-<RIGHT>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-move-row-up" "Tables" org-table-move-row-up
     "Move the current Org grid row up." "M-<UP>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-move-row-down" "Tables" org-table-move-row-down
     "Move the current Org grid row down." "M-<DOWN>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-kill-row" "Tables" org-table-kill-row
     "Delete the current Org grid row." "M-S-<UP>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-insert-row" "Tables" org-table-insert-row
     "Insert a new Org grid row." "M-S-<DOWN>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-move-cell-up" "Tables" org-table-move-cell-up
     "Move the current Org grid cell upward by swapping with its neighbor." "S-<UP>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-move-cell-down" "Tables" org-table-move-cell-down
     "Move the current Org grid cell downward by swapping with its neighbor." "S-<DOWN>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-move-cell-left" "Tables" org-table-move-cell-left
     "Move the current Org grid cell left by swapping with its neighbor." "S-<LEFT>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-move-cell-right" "Tables" org-table-move-cell-right
     "Move the current Org grid cell right by swapping with its neighbor." "S-<RIGHT>"
     ("tables" "editing") "ORG:Built-in Table Editor")
    ("org-table-hline-and-move" "Tables" org-table-hline-and-move
     "Insert a horizontal rule in an Org grid and move to the following row." "C-c <RET>"
     ("tables" "editing") "ORG:Built-in Table Editor" ("C-c RET"))
    ("org-table-sum" "Tables" org-table-sum
     "Sum the numeric cells in the current Org grid column or region." "C-c +"
     ("tables" "spreadsheet") "ORG:Built-in Table Editor")
    ("org-table-edit-formulas" "Tables" org-table-edit-formulas
     "Edit formulas for the current Org grid in a separate buffer." "C-c '"
     ("tables" "spreadsheet") "ORG:Editing and debugging formulas")
    ("org-table-field-info" "Tables" org-table-field-info
     "Display coordinate and formula information for the current Org grid field." "C-c ?"
     ("tables" "spreadsheet") "ORG:Editing and debugging formulas")
    ("org-table-recalculate" "Tables" org-table-recalculate
     "Recompute formulas for the current Org grid field or row." "C-u C-c *"
     ("tables" "spreadsheet" "prefix") "ORG:Updating the table" ("C-u C-c C-c"))
    ("org-table-iterate" "Tables" org-table-iterate
     "Iterate all formulas in the current Org grid until stable." "C-u C-u C-c *"
     ("tables" "spreadsheet" "prefix") "ORG:Updating the table" ("C-u C-u C-c C-c"))
    ("org-table-toggle-column-width" "Tables" org-table-toggle-column-width
     "Toggle width shrinking for the current Org grid column." "C-c <TAB>"
     ("tables" "display") "ORG:Column Width and Alignment" ("C-c TAB"))
    ("org-insert-link" "Hyperlinks" org-insert-link
     "Insert or edit a hyperlink." "C-c C-l"
     ("links") "ORG:Handling Links")
    ("org-insert-file-link" "Hyperlinks" org-insert-link
     "Insert a hyperlink using file-name completion immediately." "C-u C-c C-l"
     ("links" "files" "prefix") "ORG:Handling Links")
    ("org-open-at-point" "Hyperlinks" org-open-at-point
     "Open the link, timestamp, footnote, citation, or other object at point." "C-c C-o"
     ("links" "open") "ORG:Handling Links")
    ("org-next-link" "Hyperlinks" org-next-link
     "Move to the next link in the buffer." "C-c C-x C-n"
     ("links" "motion") "ORG:Handling Links")
    ("org-previous-link" "Hyperlinks" org-previous-link
     "Move to the previous link in the buffer." "C-c C-x C-p"
     ("links" "motion") "ORG:Handling Links")
    ("org-mark-ring-push" "Hyperlinks" org-mark-ring-push
     "Store the current position for later return from a followed link." "C-c %"
     ("links" "navigation") "ORG:Handling Links")
    ("org-mark-ring-goto" "Hyperlinks" org-mark-ring-goto
     "Return to a position previously stored by Org link navigation." "C-c &"
     ("links" "navigation") "ORG:Handling Links")
    ("org-todo" "TODO items" org-todo
     "Rotate the TODO state of the current heading." "C-c C-t"
     ("todo") "ORG:TODO Basics")
    ("org-todo-nextset" "TODO items" org-todo
     "Move through TODO keyword sets for the current heading." "C-u C-u C-c C-t"
     ("todo" "prefix") "ORG:Multiple sets in one file")
    ("org-shiftright" "TODO items" org-shiftright
     "Move to the next TODO state when point is on a heading state keyword." "S-<RIGHT>"
     ("todo") "ORG:TODO Basics")
    ("org-shiftleft" "TODO items" org-shiftleft
     "Move to the previous TODO state when point is on a heading state keyword." "S-<LEFT>"
     ("todo") "ORG:TODO Basics")
    ("org-insert-todo-heading-respect-content-alt" "TODO items" org-insert-todo-heading-respect-content
     "Insert a new TODO heading after the current subtree." "S-M-RET"
     ("todo" "editing") "ORG:TODO Basics" ("M-S-RET"))
    ("org-priority" "TODO items" org-priority
     "Set the priority cookie for the current heading." "C-c ,"
     ("todo" "priority") "ORG:Priorities")
    ("org-priority-up" "TODO items" org-priority-up
     "Increase the priority of the current heading." "S-<UP>"
     ("todo" "priority") "ORG:Priorities")
    ("org-priority-down" "TODO items" org-priority-down
     "Decrease the priority of the current heading." "S-<DOWN>"
     ("todo" "priority") "ORG:Priorities")
    ("org-update-statistics-cookies" "TODO items" org-update-statistics-cookies
     "Update checkbox and TODO statistics cookies in the current entry." "C-c #"
     ("todo" "checkboxes") "ORG:Checkboxes")
    ("org-toggle-checkbox" "TODO items" org-toggle-checkbox
     "Toggle the checkbox at point." "C-c C-c"
     ("checkboxes") "ORG:Checkboxes")
    ("org-insert-checkbox" "TODO items" org-insert-todo-heading
     "Insert a new checkbox item in a plain list." "M-S-RET"
     ("checkboxes" "lists") "ORG:Checkboxes")
    ("org-toggle-ordered-property" "TODO items" org-toggle-ordered-property
     "Toggle whether child TODOs or checkboxes must be completed in order." "C-c C-x o"
     ("todo" "checkboxes" "properties") "ORG:TODO dependencies")
    ("org-set-tags-command" "Tags" org-set-tags-command
     "Edit the tags on the current heading." "C-c C-q"
     ("tags") "ORG:Setting Tags")
    ("org-set-tags-command-on-headline" "Tags" org-set-tags-command
     "Edit heading tags from a headline line." "C-c C-c"
     ("tags") "ORG:Setting Tags")
    ("org-tags-completion" "Tags" pcomplete
     "Complete a tag name in Org's tag prompt." "M-TAB"
     ("tags" "completion") "ORG:Setting Tags" ("C-M-i" "<ESC> <TAB>"))
    ("org-set-property" "Properties and columns" org-set-property
     "Set a property on the current entry." "C-c C-x p"
     ("properties") "ORG:Property Syntax")
    ("org-property-action" "Properties and columns" org-property-action
     "Open Org's property action menu at point." "C-c C-c"
     ("properties") "ORG:Property Syntax")
    ("org-property-set-from-menu" "Properties and columns" org-set-property
     "Choose the property action that sets a property." "C-c C-c s"
     ("properties") "ORG:Property Syntax")
    ("org-property-delete-from-menu" "Properties and columns" org-delete-property
     "Choose the property action that deletes one property." "C-c C-c d"
     ("properties") "ORG:Property Syntax")
    ("org-property-delete-globally" "Properties and columns" org-delete-property-globally
     "Choose the property action that deletes a property everywhere." "C-c C-c D"
     ("properties") "ORG:Property Syntax")
    ("org-property-compute" "Properties and columns" org-compute-property-at-point
     "Choose the property action that computes the property at point." "C-c C-c c"
     ("properties") "ORG:Property Syntax")
    ("org-columns" "Properties and columns" org-columns
     "Enter column view for the current Org buffer or subtree." "C-c C-x C-c"
     ("properties" "columns") "ORG:Using column view")
    ("org-columns-quit" "Properties and columns" org-columns-quit
     "Leave column view from a column view line." "q"
     ("properties" "columns") "ORG:Using column view")
    ("org-columns-next-allowed-value" "Properties and columns" org-columns-next-allowed-value
     "Move a column view field to its next allowed value." "S-<RIGHT>"
     ("properties" "columns") "ORG:Using column view")
    ("org-columns-previous-allowed-value" "Properties and columns" org-columns-previous-allowed-value
     "Move a column view field to its previous allowed value." "S-<LEFT>"
     ("properties" "columns") "ORG:Using column view")
    ("org-time-stamp" "Dates and times" org-time-stamp
     "Insert an active timestamp." "C-c ."
     ("dates") "ORG:Creating Timestamps")
    ("org-time-stamp-inactive" "Dates and times" org-time-stamp-inactive
     "Insert an inactive timestamp." "C-c !"
     ("dates") "ORG:Creating Timestamps")
    ("org-time-stamp-with-time" "Dates and times" org-time-stamp
     "Insert an active timestamp that includes clock time." "C-u C-c ."
     ("dates" "prefix") "ORG:Creating Timestamps")
    ("org-date-from-calendar" "Dates and times" org-date-from-calendar
     "Insert the date selected in the calendar." "C-c <"
     ("dates" "calendar") "ORG:Creating Timestamps")
    ("org-goto-calendar-date" "Dates and times" org-goto-calendar
     "Open the calendar at the date under point." "C-c >"
     ("dates" "calendar") "ORG:Creating Timestamps")
    ("org-open-calendar-at-point" "Dates and times" org-open-at-point
     "Open the agenda or calendar view for the timestamp at point." "C-c C-o"
     ("dates" "calendar") "ORG:Creating Timestamps")
    ("org-deadline" "Dates and times" org-deadline
     "Set a deadline for the current heading." "C-c C-d"
     ("dates" "agenda") "ORG:Inserting deadline/schedule")
    ("org-schedule" "Dates and times" org-schedule
     "Schedule the current heading." "C-c C-s"
     ("dates" "agenda") "ORG:Inserting deadline/schedule")
    ("org-timestamp-up-day" "Dates and times" org-shiftup
     "Increase the date component at point." "S-<UP>"
     ("dates") "ORG:Creating Timestamps")
    ("org-timestamp-down-day" "Dates and times" org-shiftdown
     "Decrease the date component at point." "S-<DOWN>"
     ("dates") "ORG:Creating Timestamps")
    ("org-timestamp-left" "Dates and times" org-shiftleft
     "Move the timestamp at point backward in time." "S-<LEFT>"
     ("dates") "ORG:Creating Timestamps")
    ("org-timestamp-right" "Dates and times" org-shiftright
     "Move the timestamp at point forward in time." "S-<RIGHT>"
     ("dates") "ORG:Creating Timestamps")
    ("org-clock-in" "Clocking and timers" org-clock-in
     "Start clocking work on the current item." "C-c C-x C-i"
     ("clock") "ORG:Clocking commands")
    ("org-clock-in-recent" "Clocking and timers" org-clock-in
     "Start clocking by selecting from recently clocked tasks." "C-u C-c C-x C-i"
     ("clock" "prefix") "ORG:Clocking commands")
    ("org-clock-out" "Clocking and timers" org-clock-out
     "Stop the running Org clock." "C-c C-x C-o"
     ("clock") "ORG:Clocking commands")
    ("org-clock-in-last" "Clocking and timers" org-clock-in-last
     "Restart clocking the most recent task." "C-c C-x C-x"
     ("clock") "ORG:Clocking commands")
    ("org-clock-modify-effort-estimate" "Clocking and timers" org-clock-modify-effort-estimate
     "Change the effort estimate for the current clock task." "C-c C-x C-e"
     ("clock" "effort") "ORG:Clocking commands")
    ("org-evaluate-time-range" "Clocking and timers" org-evaluate-time-range
     "Recompute the duration of the time range at point." "C-c C-y"
     ("clock" "time-range") "ORG:Clocking commands" ("C-c C-c"))
    ("org-clock-cancel" "Clocking and timers" org-clock-cancel
     "Cancel a clock that was started by mistake." "C-c C-x C-q"
     ("clock") "ORG:Clocking commands")
    ("org-clock-goto" "Clocking and timers" org-clock-goto
     "Jump to the currently clocked task." "C-c C-x C-j"
     ("clock" "motion") "ORG:Clocking commands")
    ("org-clock-display" "Clocking and timers" org-clock-display
     "Show clocked-time totals as overlays in the current buffer." "C-c C-x C-d"
     ("clock" "display") "ORG:Clocking commands")
    ("org-timer-start" "Clocking and timers" org-timer-start
     "Start a relative timer." "C-c C-x 0"
     ("timer") "ORG:Timers")
    ("org-timer" "Clocking and timers" org-timer
     "Insert the current relative timer value." "C-c C-x ."
     ("timer") "ORG:Timers")
    ("org-timer-pause-or-continue" "Clocking and timers" org-timer-pause-or-continue
     "Pause or continue the relative timer." "C-c C-x ,"
     ("timer") "ORG:Timers")
    ("org-timer-item" "Clocking and timers" org-timer-item
     "Insert a timer list item." "M-RET"
     ("timer" "lists") "ORG:Timers")
    ("org-refile" "Refiling and archiving" org-refile
     "Move the current entry or region to another heading." "C-c C-w"
     ("refile") "ORG:Refile and Copy")
    ("org-refile-goto" "Refiling and archiving" org-refile
     "Use the refile interface to jump to a target heading." "C-u C-c C-w"
     ("refile" "motion" "prefix") "ORG:Refile and Copy")
    ("org-refile-goto-last-stored" "Refiling and archiving" org-refile-goto-last-stored
     "Jump to the last location used as a refile destination." "C-u C-u C-c C-w"
     ("refile" "motion" "prefix") "ORG:Refile and Copy")
    ("org-refile-copy" "Refiling and archiving" org-refile-copy
     "Copy the current entry or region to another heading through the refile interface." "C-c M-w"
     ("refile" "copy") "ORG:Refile and Copy")
    ("org-refile-reverse" "Refiling and archiving" org-refile-reverse
     "Refile while temporarily reversing whether new entries prepend or append at the target." "C-c C-M-w"
     ("refile") "ORG:Refile and Copy")
    ("org-archive-subtree-default" "Refiling and archiving" org-archive-subtree-default
     "Archive the current entry using the configured default archive command." "C-c C-x C-a"
     ("archive") "ORG:Archiving")
    ("org-archive-subtree" "Refiling and archiving" org-archive-subtree
     "Move the current subtree to its archive location." "C-c C-x C-s"
     ("archive") "ORG:Moving subtrees" ("C-c $"))
    ("org-archive-subtree-children" "Refiling and archiving" org-archive-subtree
     "Check direct child subtrees for archiving eligibility." "C-u C-c C-x C-s"
     ("archive" "prefix") "ORG:Moving subtrees")
    ("org-toggle-archive-tag" "Refiling and archiving" org-toggle-archive-tag
     "Toggle the archive tag on the current heading." "C-c C-x a"
     ("archive" "tags") "ORG:Internal archiving")
    ("org-cycle-force-archived" "Refiling and archiving" org-cycle-force-archived
     "Force visibility cycling on an archived subtree." "C-c C-TAB"
     ("archive" "visibility") "ORG:Internal archiving" ("C-c C-<TAB>"))
    ("org-capture" "Capture and attachments" org-capture
     "Start an Org capture." "C-c c"
     ("capture") "ORG:Using capture")
    ("org-capture-finalize" "Capture and attachments" org-capture-finalize
     "Finalize the current capture." "C-c C-c"
     ("capture") "ORG:Using capture")
    ("org-capture-kill" "Capture and attachments" org-capture-kill
     "Abort the current capture." "C-c C-k"
     ("capture" "quit") "ORG:Using capture")
    ("org-capture-refile" "Capture and attachments" org-capture-refile
     "Refile the current capture before finalizing it." "C-c C-w"
     ("capture" "refile") "ORG:Using capture")
    ("org-attach" "Capture and attachments" org-attach
     "Open the attachment dispatcher for the current entry." "C-c C-a"
     ("attachments") "ORG:Attachment defaults and dispatcher")
    ("org-attach-attach" "Capture and attachments" org-attach-attach
     "Attach a file by copying it into the entry attachment directory." "C-c C-a a"
     ("attachments") "ORG:Attachment defaults and dispatcher")
    ("org-attach-new" "Capture and attachments" org-attach-new
     "Create a new attachment buffer for the current entry." "C-c C-a n"
     ("attachments") "ORG:Attachment defaults and dispatcher")
    ("org-attach-open" "Capture and attachments" org-attach-open
     "Open an attachment for the current entry." "C-c C-a o"
     ("attachments" "open") "ORG:Attachment defaults and dispatcher")
    ("org-attach-open-in-emacs" "Capture and attachments" org-attach-open-in-emacs
     "Open an attachment for the current entry inside Emacs." "C-c C-a O"
     ("attachments" "open") "ORG:Attachment defaults and dispatcher")
    ("org-attach-reveal" "Capture and attachments" org-attach-reveal
     "Reveal the current entry's attachment directory." "C-c C-a f"
     ("attachments" "files") "ORG:Attachment defaults and dispatcher")
    ("org-attach-delete-one" "Capture and attachments" org-attach-delete-one
     "Delete one attachment from the current entry." "C-c C-a d"
     ("attachments" "delete") "ORG:Attachment defaults and dispatcher")
    ("org-attach-delete-all" "Capture and attachments" org-attach-delete-all
     "Delete all attachments from the current entry." "C-c C-a D"
     ("attachments" "delete") "ORG:Attachment defaults and dispatcher")
    ("org-agenda-file-to-front" "Agenda" org-agenda-file-to-front
     "Add the current file to the front of the agenda file list." "C-c ["
     ("agenda" "files") "ORG:Agenda Files")
    ("org-remove-file" "Agenda" org-remove-file
     "Remove the current file from the agenda file list." "C-c ]"
     ("agenda" "files") "ORG:Agenda Files")
    ("org-cycle-agenda-files" "Agenda" org-cycle-agenda-files
     "Cycle through files in the agenda file list." "C-'"
     ("agenda" "files") "ORG:Agenda Files")
    ("org-switchb" "Agenda" org-switchb
     "Switch to an Org buffer from the agenda file list." "C-,"
     ("agenda" "files") "ORG:Agenda Files")
    ("org-agenda" "Agenda" org-agenda
     "Open the agenda dispatcher." "C-c a"
     ("agenda") "ORG:Agenda Dispatcher")
    ("org-agenda-list-dispatch" "Agenda" org-agenda-list
     "Choose the weekly or daily agenda from the agenda dispatcher." "a"
     ("agenda" "dispatcher") "ORG:Weekly/daily agenda")
    ("org-todo-list-dispatch" "Agenda" org-todo-list
     "Choose the global TODO list from the agenda dispatcher." "t"
     ("agenda" "dispatcher" "todo") "ORG:Global TODO list")
    ("org-tags-view-dispatch" "Agenda" org-tags-view
     "Choose a tag or property match from the agenda dispatcher." "m"
     ("agenda" "dispatcher" "tags") "ORG:Matching tags and properties")
    ("org-search-view-dispatch" "Agenda" org-search-view
     "Choose a text search from the agenda dispatcher." "s"
     ("agenda" "dispatcher" "search") "ORG:Search view")
    ("org-agenda-later" "Agenda" org-agenda-later
     "Shift the agenda view forward in time." "f"
     ("agenda" "motion") "ORG:Agenda Commands")
    ("org-agenda-earlier" "Agenda" org-agenda-earlier
     "Shift the agenda view backward in time." "b"
     ("agenda" "motion") "ORG:Agenda Commands")
    ("org-agenda-goto-today" "Agenda" org-agenda-goto-today
     "Move the agenda view to today." "."
     ("agenda" "motion") "ORG:Agenda Commands")
    ("org-agenda-next-line" "Agenda" org-agenda-next-line
     "Move to the next agenda line." "n"
     ("agenda" "motion") "ORG:Agenda Commands")
    ("org-agenda-previous-line" "Agenda" org-agenda-previous-line
     "Move to the previous agenda line." "p"
     ("agenda" "motion") "ORG:Agenda Commands")
    ("org-agenda-goto" "Agenda" org-agenda-goto
     "Jump from an agenda item to its source location." "RET"
     ("agenda" "motion") "ORG:Agenda Commands")
    ("org-agenda-show" "Agenda" org-agenda-show
     "Display the source location for an agenda item in another window." "SPC"
     ("agenda" "motion") "ORG:Agenda Commands")
    ("org-agenda-redo" "Agenda" org-agenda-redo
     "Rebuild the current agenda view." "r"
     ("agenda") "ORG:Agenda Commands")
    ("org-agenda-redo-all" "Agenda" org-agenda-redo-all
     "Rebuild all agenda buffers." "g"
     ("agenda") "ORG:Agenda Commands")
    ("org-agenda-todo" "Agenda" org-agenda-todo
     "Change the TODO state of the item at point in the agenda." "t"
     ("agenda" "todo") "ORG:Agenda Commands")
    ("org-agenda-schedule" "Agenda" org-agenda-schedule
     "Schedule the agenda item at point." "C-c C-s"
     ("agenda" "dates") "ORG:Agenda Commands")
    ("org-agenda-deadline" "Agenda" org-agenda-deadline
     "Set a deadline for the agenda item at point." "C-c C-d"
     ("agenda" "dates") "ORG:Agenda Commands")
    ("org-agenda-clock-in" "Agenda" org-agenda-clock-in
     "Start clocking the agenda item at point." "I"
     ("agenda" "clock") "ORG:Agenda Commands")
    ("org-agenda-clock-out" "Agenda" org-agenda-clock-out
     "Stop the running clock from the agenda." "O"
     ("agenda" "clock") "ORG:Agenda Commands")
    ("org-agenda-clock-goto" "Agenda" org-agenda-clock-goto
     "Jump from the agenda to the currently clocked task." "J"
     ("agenda" "clock") "ORG:Agenda Commands")
    ("org-agenda-quit" "Agenda" org-agenda-quit
     "Leave the agenda view." "q"
     ("agenda" "quit") "ORG:Agenda Commands")
    ("org-agenda-exit" "Agenda" org-agenda-exit
     "Close all agenda buffers." "x"
     ("agenda" "quit") "ORG:Agenda Commands")
    ("org-agenda-filter" "Agenda" org-agenda-filter
     "Open the agenda filter dispatcher." "/"
     ("agenda" "filter") "ORG:Agenda Commands")
    ("org-agenda-filter-by-tag" "Agenda" org-agenda-filter-by-tag
     "Filter the agenda by tag." "/ t"
     ("agenda" "filter" "tags") "ORG:Agenda Commands")
    ("org-agenda-limit-to-worktree" "Agenda" org-agenda-tree-to-indirect-buffer
     "Restrict agenda commands to the item at point and its subtree." "<"
     ("agenda" "narrowing") "ORG:Agenda Commands")
    ("org-agenda-remove-restriction-lock" "Agenda" org-agenda-remove-restriction-lock
     "Remove the current agenda restriction lock." ">"
     ("agenda" "narrowing") "ORG:Agenda Commands")
    ("org-agenda-day-view" "Agenda" org-agenda-day-view
     "Switch the agenda to a daily span." "d"
     ("agenda" "view") "ORG:Agenda Commands")
    ("org-agenda-week-view" "Agenda" org-agenda-week-view
     "Switch the agenda to a weekly span." "w"
     ("agenda" "view") "ORG:Agenda Commands")
    ("org-agenda-month-view" "Agenda" org-agenda-month-view
     "Switch the agenda to a monthly span." "m"
     ("agenda" "view") "ORG:Agenda Commands")
    ("org-agenda-year-view" "Agenda" org-agenda-year-view
     "Switch the agenda to a yearly span." "y"
     ("agenda" "view") "ORG:Agenda Commands")
    ("org-agenda-log-mode" "Agenda" org-agenda-log-mode
     "Toggle logbook-related entries in the agenda." "l"
     ("agenda" "view") "ORG:Agenda Commands")
    ("org-agenda-archives-mode" "Agenda" org-agenda-archives-mode
     "Toggle archived-tree inclusion in the agenda." "v a"
     ("agenda" "archive" "view") "ORG:Agenda Commands")
    ("org-agenda-write" "Agenda" org-agenda-write
     "Write the current agenda view to a file." "C-x C-w"
     ("agenda" "export") "ORG:Exporting Agenda Views")
    ("org-latex-preview" "Markup" org-latex-preview
     "Preview LaTeX fragments in the current Org buffer." "C-c C-x C-l"
     ("markup" "latex") "ORG:Previewing LaTeX fragments")
    ("org-footnote-new" "Markup" org-footnote-new
     "Create a new footnote definition or reference." "C-c C-x f"
     ("markup" "footnotes") "ORG:Creating Footnotes")
    ("org-footnote-action" "Markup" org-footnote-action
     "Open the footnote action command for the footnote at point." "C-c C-c"
     ("markup" "footnotes") "ORG:Creating Footnotes")
    ("org-export-dispatch" "Export and publishing" org-export-dispatch
     "Open the export dispatcher." "C-c C-e"
     ("export") "ORG:The Export Dispatcher")
    ("org-export-async-toggle" "Export and publishing" org-export-dispatch
     "Toggle asynchronous export from the export dispatcher." "C-c C-e C-a"
     ("export") "ORG:The Export Dispatcher")
    ("org-export-body-only-toggle" "Export and publishing" org-export-dispatch
     "Toggle body-only export from the export dispatcher." "C-c C-e C-b"
     ("export") "ORG:The Export Dispatcher")
    ("org-export-force-publishing-toggle" "Export and publishing" org-export-dispatch
     "Toggle force-publishing from the export dispatcher." "C-c C-e C-f"
     ("export" "publishing") "ORG:The Export Dispatcher")
    ("org-export-subtree-toggle" "Export and publishing" org-export-dispatch
     "Toggle subtree-only export from the export dispatcher." "C-c C-e C-s"
     ("export") "ORG:The Export Dispatcher")
    ("org-export-visible-toggle" "Export and publishing" org-export-dispatch
     "Toggle visible-only export from the export dispatcher." "C-c C-e C-v"
     ("export") "ORG:The Export Dispatcher")
    ("org-html-export-to-html" "Export and publishing" org-html-export-to-html
     "Export the buffer to an HTML file." "C-c C-e h h"
     ("export" "html") "ORG:HTML export commands")
    ("org-html-export-as-html" "Export and publishing" org-html-export-as-html
     "Export the buffer to a temporary HTML buffer." "C-c C-e h H"
     ("export" "html") "ORG:HTML export commands")
    ("org-html-export-and-open" "Export and publishing" org-html-export-to-html
     "Export the buffer to HTML and open the result." "C-c C-e h o"
     ("export" "html" "open") "ORG:HTML export commands")
    ("org-latex-export-to-latex" "Export and publishing" org-latex-export-to-latex
     "Export the buffer to a LaTeX file." "C-c C-e l l"
     ("export" "latex") "ORG:LaTeX/PDF export commands")
    ("org-latex-export-as-latex" "Export and publishing" org-latex-export-as-latex
     "Export the buffer to a temporary LaTeX buffer." "C-c C-e l L"
     ("export" "latex") "ORG:LaTeX/PDF export commands")
    ("org-latex-export-to-pdf" "Export and publishing" org-latex-export-to-pdf
     "Export the buffer to a PDF file." "C-c C-e l p"
     ("export" "latex" "pdf") "ORG:LaTeX/PDF export commands")
    ("org-latex-export-and-open" "Export and publishing" org-latex-export-to-pdf
     "Export the buffer to PDF and open the result." "C-c C-e l o"
     ("export" "latex" "pdf" "open") "ORG:LaTeX/PDF export commands")
    ("org-beamer-export-to-latex" "Export and publishing" org-beamer-export-to-latex
     "Export the buffer to a Beamer LaTeX file." "C-c C-e l b"
     ("export" "beamer") "ORG:Beamer export commands")
    ("org-beamer-export-to-pdf" "Export and publishing" org-beamer-export-to-pdf
     "Export the buffer to a Beamer PDF file." "C-c C-e l P"
     ("export" "beamer" "pdf") "ORG:Beamer export commands")
    ("org-ascii-export-to-ascii" "Export and publishing" org-ascii-export-to-ascii
     "Export the buffer to an ASCII file." "C-c C-e t a"
     ("export" "text") "ORG:ASCII/Latin-1/UTF-8 export")
    ("org-utf8-export-to-utf8" "Export and publishing" org-ascii-export-to-ascii
     "Export the buffer to a UTF-8 text file." "C-c C-e t u"
     ("export" "text") "ORG:ASCII/Latin-1/UTF-8 export")
    ("org-md-export-to-markdown" "Export and publishing" org-md-export-to-markdown
     "Export the buffer to a Markdown file." "C-c C-e m m"
     ("export" "markdown") "ORG:Markdown Export")
    ("org-odt-export-to-odt" "Export and publishing" org-odt-export-to-odt
     "Export the buffer to an OpenDocument Text file." "C-c C-e o o"
     ("export" "odt") "ORG:ODT export commands")
    ("org-org-export-to-org" "Export and publishing" org-org-export-to-org
     "Export the buffer to another Org file." "C-c C-e O o"
     ("export" "org") "ORG:Org Export")
    ("org-texinfo-export-to-texinfo" "Export and publishing" org-texinfo-export-to-texinfo
     "Export the buffer to a Texinfo file." "C-c C-e i t"
     ("export" "texinfo") "ORG:Texinfo export commands")
    ("org-icalendar-export-to-ics" "Export and publishing" org-icalendar-export-to-ics
     "Export the buffer to an iCalendar file." "C-c C-e c f"
     ("export" "icalendar") "ORG:iCalendar Export")
    ("org-publish" "Export and publishing" org-publish
     "Publish one project after selecting it." "C-c C-e P x"
     ("publishing") "ORG:Triggering Publication")
    ("org-publish-current-project" "Export and publishing" org-publish-current-project
     "Publish the current project." "C-c C-e P p"
     ("publishing") "ORG:Triggering Publication")
    ("org-publish-current-file" "Export and publishing" org-publish-current-file
     "Publish the current file." "C-c C-e P f"
     ("publishing") "ORG:Triggering Publication")
    ("org-publish-all" "Export and publishing" org-publish-all
     "Publish every configured project." "C-c C-e P a"
     ("publishing") "ORG:Triggering Publication")
    ("org-edit-special" "Source code" org-edit-special
     "Edit the source block, example block, or export block at point in a dedicated buffer." "C-c '"
     ("source" "editing") "ORG:Editing Source Code")
    ("org-edit-src-exit" "Source code" org-edit-src-exit
     "Finish editing an Org source block in its edit buffer." "C-c C-c"
     ("source" "editing") "ORG:Editing Source Code")
    ("org-edit-src-abort" "Source code" org-edit-src-abort
     "Abort editing an Org source block in its edit buffer." "C-c C-k"
     ("source" "editing" "quit") "ORG:Editing Source Code")
    ("org-babel-execute-src-block" "Source code" org-babel-execute-src-block
     "Execute the current source block." "C-c C-c"
     ("source" "babel" "execute") "ORG:Evaluating Code Blocks")
    ("org-babel-execute-maybe" "Source code" org-babel-execute-maybe
     "Execute the source block at point through Babel's command map." "C-c C-v e"
     ("source" "babel" "execute") "ORG:Key bindings and Useful Functions" ("C-c C-v C-e"))
    ("org-babel-next-src-block" "Source code" org-babel-next-src-block
     "Move to the next source block." "C-c C-v n"
     ("source" "babel" "motion") "ORG:Key bindings and Useful Functions" ("C-c C-v C-n"))
    ("org-babel-previous-src-block" "Source code" org-babel-previous-src-block
     "Move to the previous source block." "C-c C-v p"
     ("source" "babel" "motion") "ORG:Key bindings and Useful Functions" ("C-c C-v C-p"))
    ("org-babel-open-src-block-result" "Source code" org-babel-open-src-block-result
     "Open the result of the current source block." "C-c C-v o"
     ("source" "babel" "results") "ORG:Key bindings and Useful Functions" ("C-c C-v C-o"))
    ("org-babel-expand-src-block" "Source code" org-babel-expand-src-block
     "Expand the current source block with variables and header arguments." "C-c C-v v"
     ("source" "babel") "ORG:Key bindings and Useful Functions" ("C-c C-v C-v"))
    ("org-babel-goto-src-block-head" "Source code" org-babel-goto-src-block-head
     "Move to the header line of the current source block." "C-c C-v u"
     ("source" "babel" "motion") "ORG:Key bindings and Useful Functions" ("C-c C-v C-u"))
    ("org-babel-goto-named-src-block" "Source code" org-babel-goto-named-src-block
     "Jump to a named source block." "C-c C-v g"
     ("source" "babel" "motion") "ORG:Key bindings and Useful Functions" ("C-c C-v C-g"))
    ("org-babel-goto-named-result" "Source code" org-babel-goto-named-result
     "Jump to a named source block result." "C-c C-v r"
     ("source" "babel" "motion") "ORG:Key bindings and Useful Functions" ("C-c C-v C-r"))
    ("org-babel-execute-buffer" "Source code" org-babel-execute-buffer
     "Execute every source block in the current buffer." "C-c C-v b"
     ("source" "babel" "execute") "ORG:Key bindings and Useful Functions" ("C-c C-v C-b"))
    ("org-babel-execute-subtree" "Source code" org-babel-execute-subtree
     "Execute every source block in the current subtree." "C-c C-v s"
     ("source" "babel" "execute") "ORG:Key bindings and Useful Functions" ("C-c C-v C-s"))
    ("org-babel-demarcate-block" "Source code" org-babel-demarcate-block
     "Split the current source block at point." "C-c C-v d"
     ("source" "babel" "editing") "ORG:Key bindings and Useful Functions" ("C-c C-v C-d"))
    ("org-babel-tangle" "Source code" org-babel-tangle
     "Tangle source blocks from the current file." "C-c C-v t"
     ("source" "babel" "tangle") "ORG:Extracting Source Code" ("C-c C-v C-t"))
    ("org-babel-tangle-file" "Source code" org-babel-tangle-file
     "Choose a file and tangle its source blocks." "C-c C-v f"
     ("source" "babel" "tangle") "ORG:Extracting Source Code" ("C-c C-v C-f"))
    ("org-babel-check-src-block" "Source code" org-babel-check-src-block
     "Check the current source block for syntax and header issues." "C-c C-v c"
     ("source" "babel") "ORG:Key bindings and Useful Functions" ("C-c C-v C-c"))
    ("org-babel-insert-header-arg" "Source code" org-babel-insert-header-arg
     "Insert a source-block header argument." "C-c C-v j"
     ("source" "babel" "headers") "ORG:Key bindings and Useful Functions" ("C-c C-v C-j"))
    ("org-babel-load-in-session" "Source code" org-babel-load-in-session
     "Load the current source block into a language session." "C-c C-v l"
     ("source" "babel" "session") "ORG:Key bindings and Useful Functions" ("C-c C-v C-l"))
    ("org-babel-lob-ingest" "Source code" org-babel-lob-ingest
     "Add source blocks from a file to the Library of Babel." "C-c C-v i"
     ("source" "babel" "library") "ORG:Library of Babel" ("C-c C-v C-i"))
    ("org-babel-view-src-block-info" "Source code" org-babel-view-src-block-info
     "Show information about the current source block." "C-c C-v I"
     ("source" "babel") "ORG:Key bindings and Useful Functions" ("C-c C-v C-I"))
    ("org-babel-switch-to-session-with-code" "Source code" org-babel-switch-to-session-with-code
     "Switch to the current source block's session with its code inserted." "C-c C-v z"
     ("source" "babel" "session") "ORG:Key bindings and Useful Functions" ("C-c C-v C-z"))
    ("org-babel-sha1-hash" "Source code" org-babel-sha1-hash
     "Compute the SHA1 hash of the current source block." "C-c C-v a"
     ("source" "babel") "ORG:Key bindings and Useful Functions" ("C-c C-v C-a"))
    ("org-babel-describe-bindings" "Source code" org-babel-describe-bindings
     "Show Babel key bindings and useful functions." "C-c C-v h"
     ("source" "babel" "help") "ORG:Key bindings and Useful Functions" ("C-c C-v C-h"))
    ("org-babel-do-key-sequence-in-edit-buffer" "Source code" org-babel-do-key-sequence-in-edit-buffer
     "Run a key sequence in the source block edit buffer from the Org buffer." "C-c C-v x"
     ("source" "babel" "editing") "ORG:Key bindings and Useful Functions" ("C-c C-v C-x"))
    ("org-ctrl-c-ctrl-c" "Miscellaneous" org-ctrl-c-ctrl-c
     "Run Org's context-sensitive action at point." "C-c C-c"
     ("context") "ORG:The Very Busy C-c C-c Key")
    ("org-reload" "Miscellaneous" org-reload
     "Reload Org after updating it in the current Emacs session." "C-u M-x org-reload RET"
     ("maintenance" "prefix" "extended-command") "ORG:Installation"
     ("C-u <ESC> x org-reload RET")
     (:display-answer "C-u M-x org-reload RET"))
    ("org-version" "Miscellaneous" org-version
     "Show the installed Org version by command name." "M-x org-version RET"
     ("maintenance" "extended-command") "ORG:Feedback"
     ("<ESC> x org-version RET")
     (:display-answer "M-x org-version RET"))
    ("org-submit-bug-report" "Miscellaneous" org-submit-bug-report
     "Compose an Org bug report by command name." "M-x org-submit-bug-report RET"
     ("maintenance" "extended-command") "ORG:Feedback"
     ("<ESC> x org-submit-bug-report RET")
     (:display-answer "M-x org-submit-bug-report RET")))
  "Curated card specs derived from the installed Org manual.")

(defconst emacs-srs-trainer-org-manual-cards
  (mapcar (lambda (spec)
            (apply #'emacs-srs-trainer-deck--org-card spec))
          emacs-srs-trainer-org-manual-card-specs)
  "Curated deck derived from the installed Org manual.")

(emacs-srs-trainer-register-deck
 emacs-srs-trainer-tutorial-deck-name
 emacs-srs-trainer-tutorial-cards)

(emacs-srs-trainer-register-deck
 emacs-srs-trainer-info-deck-name
 emacs-srs-trainer-info-introduction-cards)

(emacs-srs-trainer-register-deck
 emacs-srs-trainer-org-deck-name
 emacs-srs-trainer-org-manual-cards)

(provide 'emacs-srs-trainer-deck)

;;; emacs-srs-trainer-deck.el ends here
