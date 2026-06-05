;;; emacs-srs-trainer-tutorial.el --- Tutorial discovery and extraction  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Locate the installed Emacs Tutorial through `data-directory' and
;; extract obvious key notations for deck coverage validation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-srs-trainer-deck)

(defgroup emacs-srs-trainer-tutorial nil
  "Installed tutorial extraction for `emacs-srs-trainer'."
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-tutorial-extra-paths nil
  "Additional tutorial paths to try before the built-in Emacs locations."
  :type '(repeat file))

(defconst emacs-srs-trainer-tutorial-ignored-keybindings
  '(("C-x" . "Prefix key; covered by concrete C-x command cards.")
    ("C-h" . "Prefix key; covered by concrete C-h command cards.")
    ("C-u" . "Prefix argument introducer; covered by numeric-prefix cards.")
    ("ESC" . "Meta-prefix notation; covered by Meta alternatives and ESC ESC ESC.")
    ("C-x 4" . "Prefix key; covered by C-x 4 C-f.")
    ("C-x 5" . "Prefix key; covered by C-x 5 2 and C-x 5 0.")
    ("M-2" . "Meta digit prefix notation; covered by C-u numeric-prefix cards.")
    ("M-3" . "Meta digit prefix notation; covered by C-u numeric-prefix cards.")
    ("M-8" . "Meta digit prefix notation; covered by C-u numeric-prefix cards.")
    ("M-0" . "Meta digit prefix notation; covered by C-u numeric-prefix cards.")
    ("SPC" . "Disabled-command prompt answer; intentionally not trained as a card."))
  "Extracted tutorial key candidates that are intentionally not standalone cards.")

(defun emacs-srs-trainer-tutorial-candidate-paths ()
  "Return tutorial paths to try in priority order."
  (append emacs-srs-trainer-tutorial-extra-paths
          (list (expand-file-name "tutorials/TUTORIAL" data-directory)
                (expand-file-name "TUTORIAL" data-directory))))

(defun emacs-srs-trainer-tutorial-file ()
  "Return the installed tutorial path, or nil when unavailable."
  (cl-find-if #'file-readable-p
              (emacs-srs-trainer-tutorial-candidate-paths)))

(defun emacs-srs-trainer-tutorial--clean-token (token)
  "Strip prose punctuation around TOKEN while preserving key notation."
  (let ((clean (string-trim token "[\"'({[]+" "[])}.,;:!\"']+")))
    (cond
     ((string-match (rx bos "<ESC>" (group (+ alpha)) eos) clean)
      (concat "M-" (match-string 1 clean)))
     (t clean))))

(defun emacs-srs-trainer-tutorial--key-token-p (token)
  "Return non-nil when TOKEN looks like Emacs key notation."
  (and (not (string-match-p (rx "<chr>") token))
       (or (string-match-p (rx bos "C-M-" (+ (not space)) eos) token)
           (string-match-p (rx bos "C-" (+ (not space)) eos) token)
           (string-match-p (rx bos "M-" (+ (not space)) eos) token)
           (and (string-match-p (rx bos "<" (+ (not (any ">"))) ">" eos) token)
                (member (downcase (substring token 1 -1))
                        '("del" "return" "tab" "spc" "esc" "escape"
                          "next" "prior" "pagedown" "pageup")))
           (member token '("RET" "DEL" "TAB" "SPC" "ESC")))))

(defun emacs-srs-trainer-tutorial--prefixed-token-p (tokens index)
  "Return non-nil when TOKENS at INDEX is part of a larger key sequence."
  (let ((previous (nth (1- index) tokens)))
    (member previous '("C-x" "C-h" "ESC" "<ESC>"))))

(defun emacs-srs-trainer-tutorial--bare-key-token-p (token)
  "Return non-nil when TOKEN can complete a prefix key notation."
  (or (string-match-p (rx bos (any "A-Za-z0-9?*") eos) token)
      (emacs-srs-trainer-tutorial--key-token-p token)))

(defun emacs-srs-trainer-tutorial--line-tokens (line-text)
  "Return cleaned whitespace-delimited tokens from LINE-TEXT."
  (delq nil
        (mapcar (lambda (token)
                  (let ((clean (emacs-srs-trainer-tutorial--clean-token token)))
                    (unless (string-empty-p clean) clean)))
                (split-string line-text "[ \t\n]+" t))))

(defun emacs-srs-trainer-tutorial--add-candidate (table key line section text)
  "Add KEY found at LINE in SECTION and TEXT to TABLE."
  (condition-case nil
      (let* ((normalized (emacs-srs-trainer-normalize-key key))
             (existing (gethash normalized table))
             (source (format "TUTORIAL:line-%d" line)))
        (if existing
            (progn
              (plist-put existing :lines (cons line (plist-get existing :lines)))
              (plist-put existing :source-refs
                         (cons source (plist-get existing :source-refs))))
          (puthash normalized
                   (list :key normalized
                         :line line
                         :lines (list line)
                         :section section
                         :source-ref source
                         :source-refs (list source)
                         :text (string-trim text))
                   table)))
    (error nil)))

(defun emacs-srs-trainer-tutorial--add-token-candidates
    (tokens line-number section line-text table)
  "Add key candidates from TOKENS on LINE-NUMBER to TABLE."
  (cl-loop for index from 0 below (length tokens)
           for token = (nth index tokens)
           do
           (when (and (emacs-srs-trainer-tutorial--key-token-p token)
                      (not (emacs-srs-trainer-tutorial--prefixed-token-p
                            tokens index)))
             (emacs-srs-trainer-tutorial--add-candidate
              table token line-number section line-text))
           (pcase token
             ((or "C-x" "C-h")
              (let ((next (nth (1+ index) tokens))
                    (third (nth (+ index 2) tokens)))
                (when (and next (emacs-srs-trainer-tutorial--bare-key-token-p next))
                  (emacs-srs-trainer-tutorial--add-candidate
                   table (format "%s %s" token next)
                   line-number section line-text))
                (when (and (string= token "C-x")
                           (member next '("4" "5"))
                           third
                           (emacs-srs-trainer-tutorial--bare-key-token-p third))
                  (emacs-srs-trainer-tutorial--add-candidate
                   table (format "%s %s %s" token next third)
                   line-number section line-text))))
             ("C-u"
              (let ((cursor (1+ index))
                    (parts (list "C-u")))
                (while (and (nth cursor tokens)
                            (string-match-p (rx bos (+ digit) eos)
                                            (nth cursor tokens)))
                  (setq parts (append parts (list (nth cursor tokens))))
                  (setq cursor (1+ cursor)))
                (let ((next (nth cursor tokens))
                      (after-next (nth (1+ cursor) tokens)))
                  (cond
                   ((and next
                         (string= next "C-x")
                         after-next
                         (emacs-srs-trainer-tutorial--bare-key-token-p after-next))
                    (emacs-srs-trainer-tutorial--add-candidate
                     table (string-join (append parts (list next after-next)) " ")
                     line-number section line-text))
                   ((and next (or (emacs-srs-trainer-tutorial--key-token-p next)
                                  (string= next "*")))
                    (emacs-srs-trainer-tutorial--add-candidate
                     table (string-join (append parts (list next)) " ")
                     line-number section line-text))))))
             ((or "<ESC>" "ESC")
              (let ((next (nth (1+ index) tokens))
                    (third (nth (+ index 2) tokens)))
                (cond
                 ((and next third
                       (member next '("<ESC>" "ESC"))
                       (member third '("<ESC>" "ESC")))
                  (emacs-srs-trainer-tutorial--add-candidate
                   table "ESC ESC ESC" line-number section line-text))
                 ((and next (string= next "C-v"))
                  (emacs-srs-trainer-tutorial--add-candidate
                   table "<ESC> C-v" line-number section line-text))))))))

(defun emacs-srs-trainer-tutorial-extract-keybindings (&optional file)
  "Extract obvious keybinding candidates from tutorial FILE.

Return a list of plists, one per unique normalized key.  If FILE is
nil, locate the installed tutorial using `data-directory'.  Missing
files return nil rather than signaling."
  (let ((path (or file (emacs-srs-trainer-tutorial-file))))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((table (make-hash-table :test 'equal))
              (line-number 0)
              (section "Preamble"))
          (goto-char (point-min))
          (while (not (eobp))
            (setq line-number (1+ line-number))
            (let ((line-text (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
              (when (string-match (rx bol "*" (+ space) (group (+ not-newline))) line-text)
                (setq section (string-trim (match-string 1 line-text))))
              (emacs-srs-trainer-tutorial--add-token-candidates
               (emacs-srs-trainer-tutorial--line-tokens line-text)
               line-number section line-text table))
            (forward-line 1))
          (sort (hash-table-values table)
                (lambda (a b)
                  (string< (plist-get a :key) (plist-get b :key)))))))))

(defun emacs-srs-trainer-tutorial-extraction-summary (&optional file)
  "Return a plist summarizing tutorial extraction for FILE."
  (let ((path (or file (emacs-srs-trainer-tutorial-file))))
    (if (and path (file-readable-p path))
        (let ((candidates (emacs-srs-trainer-tutorial-extract-keybindings path)))
          (list :ok t
                :path path
                :count (length candidates)
                :keys (mapcar (lambda (candidate)
                                (plist-get candidate :key))
                              candidates)))
      (list :ok nil
            :path path
            :count 0
            :keys nil
            :message "tutorial file not found"))))

(provide 'emacs-srs-trainer-tutorial)

;;; emacs-srs-trainer-tutorial.el ends here
