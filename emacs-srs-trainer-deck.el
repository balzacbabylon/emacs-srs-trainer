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

(defgroup emacs-srs-trainer nil
  "Emacs-native spaced repetition for Emacs keybindings."
  :group 'applications
  :prefix "emacs-srs-trainer-")

(defconst emacs-srs-trainer-version "0.1.0"
  "Version of `emacs-srs-trainer'.")

(defvar emacs-srs-trainer-decks nil
  "Registered decks as an alist of (DECK-NAME . CARDS).")

(defconst emacs-srs-trainer-required-card-fields
  '(:id :deck :topic :question :canonical-answer :tags :source-ref)
  "Required plist keys for every card.")

(defconst emacs-srs-trainer-tutorial-deck-name "Emacs Tutorial"
  "Name of the built-in Emacs Tutorial deck.")

(defconst emacs-srs-trainer-info-deck-name "Info: An Introduction"
  "Name of the built-in Info introduction deck.")

(defcustom emacs-srs-trainer-key-display-alist
  '(("DEL" . "Backspace/Delete (DEL)")
    ("M-DEL" . "M-Backspace/Delete (M-DEL)")
    ("<backspace>" . "Backspace/Delete (<backspace>)")
    ("M-<backspace>" . "M-Backspace/Delete (M-<backspace>)")
    ("<backtab>" . "Shift-TAB (<backtab>)"))
  "Alist mapping canonical key notation to friendlier display text."
  :type '(alist :key-type string :value-type string)
  :group 'emacs-srs-trainer)

(defun emacs-srs-trainer-deck--canonicalize-token (token)
  "Canonicalize one key-description TOKEN for comparison."
  (cond
   ((member token '("<return>" "<Return>")) "RET")
   ((member token '("<tab>" "<Tab>")) "TAB")
   ((member token '("<escape>" "<Escape>")) "ESC")
   ((member token '("<BACKSPACE>" "<Backspace>")) "<backspace>")
   (t token)))

(defun emacs-srs-trainer-canonicalize-key-description (description)
  "Canonicalize key DESCRIPTION returned by `key-description'."
  (mapconcat #'emacs-srs-trainer-deck--canonicalize-token
             (split-string description " " t)
             " "))

(defun emacs-srs-trainer-normalize-key (key)
  "Return canonical Emacs notation for KEY.

KEY may be a string accepted by `kbd' or a vector returned by
`read-key-sequence-vector'."
  (emacs-srs-trainer-canonicalize-key-description
   (cond
    ((vectorp key) (key-description key))
    ((stringp key) (key-description (kbd key)))
    (t (error "Unsupported key value: %S" key)))))

(defun emacs-srs-trainer-display-key (key)
  "Return a user-facing display string for KEY."
  (let ((normalized (emacs-srs-trainer-normalize-key key)))
    (or (cdr (assoc normalized emacs-srs-trainer-key-display-alist))
        normalized)))

(defun emacs-srs-trainer-card-display-answer (card)
  "Return user-facing correct answer text for CARD."
  (or (plist-get card :display-answer)
      (emacs-srs-trainer-display-key (plist-get card :canonical-answer))))

(defun emacs-srs-trainer-card-id (card)
  "Return CARD's id."
  (plist-get card :id))

(defun emacs-srs-trainer-card-topic (card)
  "Return CARD's topic."
  (plist-get card :topic))

(defun emacs-srs-trainer-card-answers (card)
  "Return CARD's canonical answer plus accepted alternatives."
  (cons (plist-get card :canonical-answer)
        (plist-get card :accepted-answers)))

(defun emacs-srs-trainer-card-normalized-answers (card)
  "Return normalized accepted answer strings for CARD."
  (delete-dups
   (mapcar #'emacs-srs-trainer-normalize-key
           (emacs-srs-trainer-card-answers card))))

(defun emacs-srs-trainer-grade-answer (card answer)
  "Grade ANSWER against CARD.

ANSWER may be a vector from `read-key-sequence-vector' or a key
notation string.  Return a plist with :correct, :answer, and
:accepted-answers."
  (let* ((normalized-answer (emacs-srs-trainer-normalize-key answer))
         (accepted (emacs-srs-trainer-card-normalized-answers card)))
    (list :correct (member normalized-answer accepted)
          :answer normalized-answer
          :accepted-answers accepted)))

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
    "Start the named-command prompt before running replace-string." "M-x"
    '("extended-command" "replace") "TUTORIAL:Extending the command set" '("<ESC> x")
    '(:command-name "replace-string"))
   (emacs-srs-trainer-deck--card
    "tutorial-recover-this-file" "Files" 'recover-this-file
    "Start the named-command prompt before recovering an auto-save file." "M-x"
    '("extended-command" "files") "TUTORIAL:Auto save" '("<ESC> x")
    '(:command-name "recover-this-file"))
   (emacs-srs-trainer-deck--card
    "tutorial-fundamental-mode" "Modes and filling" 'fundamental-mode
    "Start the named-command prompt before switching to Fundamental mode." "M-x"
    '("extended-command" "modes") "TUTORIAL:Mode line" '("<ESC> x")
    '(:command-name "fundamental-mode"))
   (emacs-srs-trainer-deck--card
    "tutorial-text-mode" "Modes and filling" 'text-mode
    "Start the named-command prompt before switching to Text mode." "M-x"
    '("extended-command" "modes") "TUTORIAL:Mode line" '("<ESC> x")
    '(:command-name "text-mode"))
   (emacs-srs-trainer-deck--card
    "tutorial-auto-fill-mode" "Modes and filling" 'auto-fill-mode
    "Start the named-command prompt before toggling Auto Fill mode." "M-x"
    '("extended-command" "modes" "filling") "TUTORIAL:Modes and filling" '("<ESC> x")
    '(:command-name "auto-fill-mode"))
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
    "Start the named-command prompt before listing installable packages." "M-x"
    '("extended-command" "packages") "TUTORIAL:Installing packages" '("<ESC> x")
    '(:command-name "list-packages"))

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
    "Start the named-command prompt before toggling visibility of hidden Info link text." "M-x"
    '("display" "extended-command") "INFO:Help-Inv" '("<ESC> x")
    '(:command-name "visible-mode"))

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
    "Start the named-command prompt before searching all installed Info indices." "M-x"
    '("advanced" "index" "extended-command") "INFO:Search Index" '("<ESC> x")
    '(:command-name "info-apropos"))
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
    "Start the named-command prompt before showing a manual by name while reusing an existing Info buffer when possible." "M-x"
    '("advanced" "buffers" "extended-command") "INFO:Create Info buffer" '("<ESC> x")
    '(:command-name "info-display-manual")))
  "Curated deck derived from the installed Info manual introduction.")

(emacs-srs-trainer-register-deck
 emacs-srs-trainer-tutorial-deck-name
 emacs-srs-trainer-tutorial-cards)

(emacs-srs-trainer-register-deck
 emacs-srs-trainer-info-deck-name
 emacs-srs-trainer-info-introduction-cards)

(provide 'emacs-srs-trainer-deck)

;;; emacs-srs-trainer-deck.el ends here
