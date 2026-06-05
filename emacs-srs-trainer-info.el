;;; emacs-srs-trainer-info.el --- Info manual discovery and extraction  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Locate the installed "Info: An Introduction" manual and extract the
;; obvious key notation it teaches.  The curated deck remains hand-authored,
;; but validation compares it with these extracted candidates.

;;; Code:

(require 'cl-lib)
(require 'info)
(require 'subr-x)
(require 'emacs-srs-trainer-deck)

(defgroup emacs-srs-trainer-info nil
  "Installed Info manual extraction for `emacs-srs-trainer'."
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-info-extra-paths nil
  "Additional Info manual paths to try before Emacs's Info directories."
  :type '(repeat file)
  :group 'emacs-srs-trainer-info)

(defconst emacs-srs-trainer-info-ignored-keybindings
  '(("H" . "Stand-alone Info reader help key; Emacs Info uses ?.")
    ("x" . "Stand-alone Info reader help-exit key; Emacs help exits automatically.")
    ("0" . "Stand-alone Info reader last-menu-item shortcut; not taught for Emacs Info.")
    ("2" . "Representative Info-nth-menu-item cards cover the digit command family.")
    ("3" . "Representative Info-nth-menu-item cards cover the digit command family.")
    ("4" . "Representative Info-nth-menu-item cards cover the digit command family.")
    ("5" . "Representative Info-nth-menu-item cards cover the digit command family.")
    ("6" . "Representative Info-nth-menu-item cards cover the digit command family.")
    ("7" . "Representative Info-nth-menu-item cards cover the digit command family.")
    ("8" . "Representative Info-nth-menu-item cards cover the digit command family.")
    ("<PageUp>" . "Hardware-specific key; Info also teaches universal backward scrolling keys.")
    ("<PageDown>" . "Hardware-specific key; Info also teaches universal forward scrolling keys.")
    ("<ENTER>" . "Keyboard label synonym for RET, which is covered.")
    ("<CTRL>" . "Modifier-key label, not a complete command.")
    ("<Shift>" . "Modifier-key label, not a complete command.")
    ("<META>" . "Modifier-key label, not a complete command."))
  "Extracted Info manual key candidates intentionally not used as cards.")

(defun emacs-srs-trainer-info--suffixes ()
  "Return file suffixes used when locating Info files."
  (mapcar (lambda (entry) (if (consp entry) (car entry) entry))
          Info-suffix-list))

(defun emacs-srs-trainer-info-candidate-paths ()
  "Return candidate paths for the installed Info introduction manual."
  (let* ((dirs (delete-dups
                (delq nil
                      (append Info-directory-list
                              (when (fboundp 'Info-default-dirs)
                                (Info-default-dirs))))))
         (located (locate-file "info" dirs
                               (emacs-srs-trainer-info--suffixes))))
    (append emacs-srs-trainer-info-extra-paths
            (delq nil (list located)))))

(defun emacs-srs-trainer-info-file ()
  "Return the installed Info introduction manual path, or nil."
  (cl-find-if #'file-readable-p
              (emacs-srs-trainer-info-candidate-paths)))

(defun emacs-srs-trainer-info--clean-token (token)
  "Return TOKEN stripped of surrounding prose punctuation."
  (let ((clean (string-trim token)))
    (setq clean (replace-regexp-in-string (rx bos "‘") "" clean))
    (setq clean (replace-regexp-in-string (rx "’" eos) "" clean))
    (setq clean (replace-regexp-in-string (rx bos "'") "" clean))
    (setq clean (replace-regexp-in-string (rx "'" eos) "" clean))
    (setq clean (replace-regexp-in-string (rx (one-or-more (any "." ";" ":" "," ")" "”")) eos) "" clean))
    clean))

(defun emacs-srs-trainer-info--canonical-spelling (token)
  "Return Info manual TOKEN in notation accepted by `kbd'."
  (let ((clean (emacs-srs-trainer-info--clean-token token)))
    (setq clean (replace-regexp-in-string "<SPC>" "SPC" clean t t))
    (setq clean (replace-regexp-in-string "<RET>" "RET" clean t t))
    (setq clean (replace-regexp-in-string "<Return>" "RET" clean t t))
    (setq clean (replace-regexp-in-string "<TAB>" "TAB" clean t t))
    (setq clean (replace-regexp-in-string "<DEL>" "DEL" clean t t))
    (setq clean (replace-regexp-in-string "Control-" "C-" clean t t))
    (cond
     ((string= clean "^L") "C-l")
     ((string= clean "<BACKSPACE>") "<backspace>")
     (t clean))))

(defun emacs-srs-trainer-info--key-token-p (token)
  "Return non-nil when TOKEN looks like a taught Info key token."
  (let ((key (emacs-srs-trainer-info--canonical-spelling token)))
    (or (member key '("h" "H" "?" "n" "p" "SPC" "DEL" "<backspace>"
                      "S-SPC" "b" "C-l" "]" "[" "m" "TAB" "<backtab>"
                      "S-<tab>" "S-TAB" "M-TAB" "M-<tab>" "C-M-i" "RET"
                      "u" "f" "l" "r" "L" "d" "t" "q" "s" "C-s" "C-r"
                      "i" "," "I" "g" "M-n" "C-q" "M-x" "x" "0"
                      "1" "2" "3" "4" "5" "6" "7" "8" "9"
                      "<PageUp>" "<PageDown>" "<ENTER>" "<CTRL>" "<Shift>"
                      "<META>"))
        (string-match-p (rx bos (or "C-" "M-" "C-M-") (+ (not space)) eos)
                        key))))

(defun emacs-srs-trainer-info--add-candidate
    (table key line node line-text)
  "Add KEY found at LINE in NODE and LINE-TEXT to TABLE."
  (condition-case nil
      (let* ((canonical (emacs-srs-trainer-info--canonical-spelling key))
             (normalized (emacs-srs-trainer-normalize-key canonical))
             (existing (gethash normalized table))
             (source (format "INFO:%s:line-%d" node line)))
        (if existing
            (progn
              (plist-put existing :lines (cons line (plist-get existing :lines)))
              (plist-put existing :source-refs
                         (cons source (plist-get existing :source-refs))))
          (puthash normalized
                   (list :key normalized
                         :line line
                         :lines (list line)
                         :node node
                         :source-ref source
                         :source-refs (list source)
                         :text (string-trim line-text))
                   table)))
    (error nil)))

(defun emacs-srs-trainer-info--add-key-if-valid
    (table key line node line-text)
  "Add KEY to TABLE when it looks like Info key notation."
  (when (emacs-srs-trainer-info--key-token-p key)
    (emacs-srs-trainer-info--add-candidate table key line node line-text)))

(defun emacs-srs-trainer-info--add-quoted-candidate
    (table quoted line node line-text)
  "Extract key candidates from QUOTED manual text."
  (let* ((text (emacs-srs-trainer-info--canonical-spelling quoted))
         (parts (split-string text "[ \t]+" t)))
    (cond
     ((string-match-p (rx bos "M-x" (or eos space)) text)
      (emacs-srs-trainer-info--add-candidate table "M-x" line node line-text))
     ((string-match-p (rx bos "C-u" space (? (+ digit) space)
                         (or "m" "g" "C-h i") eos)
                      text)
      (emacs-srs-trainer-info--add-candidate table text line node line-text))
     ((string= text "C-h i")
      (emacs-srs-trainer-info--add-candidate table "C-h i" line node line-text))
     ((string= text "C-q ?")
      (emacs-srs-trainer-info--add-candidate table "C-q ?" line node line-text)
      (emacs-srs-trainer-info--add-candidate table "C-q" line node line-text))
     ((string= text "f?")
      (emacs-srs-trainer-info--add-candidate table "f ?" line node line-text))
     ((string= text "g*RET")
      (emacs-srs-trainer-info--add-candidate table "g * RET" line node line-text))
     ((string-match (rx bos (group (any "fgmis")) (+ anything) "RET" eos) text)
      (emacs-srs-trainer-info--add-candidate
       table (match-string 1 text) line node line-text))
     ((and parts (cl-every #'emacs-srs-trainer-info--key-token-p parts))
      (if (and (<= (length parts) 4)
               (member (car parts) '("C-u" "C-q")))
          (emacs-srs-trainer-info--add-candidate
           table (string-join parts " ") line node line-text)
        (dolist (part parts)
          (emacs-srs-trainer-info--add-key-if-valid table part line node line-text))))
     ((emacs-srs-trainer-info--key-token-p text)
      (emacs-srs-trainer-info--add-candidate table text line node line-text)))))

(defun emacs-srs-trainer-info--scan-line
    (table line-text line-number node)
  "Scan LINE-TEXT at LINE-NUMBER in NODE for Info key candidates."
  (let ((start 0))
    (while (string-match "\\(?:C-M-\\|[CMS]-\\)?<[^>\n]+>"
                         line-text start)
      (let ((token (match-string 0 line-text))
            (next-start (match-end 0)))
        (emacs-srs-trainer-info--add-key-if-valid
         table token line-number node line-text)
        (setq start next-start))))
  (let ((start 0))
    (while (string-match "‘\\([^’\n]+\\)’" line-text start)
      (let ((quoted (match-string 1 line-text))
            (next-start (match-end 0)))
        (emacs-srs-trainer-info--add-quoted-candidate
         table quoted line-number node line-text)
        (setq start next-start)))))

(defun emacs-srs-trainer-info-extract-keybindings (&optional file)
  "Extract obvious keybinding candidates from the Info introduction FILE.

Return a list of plists, one per unique normalized key.  If FILE is nil,
locate the installed Info manual.  Missing files return nil."
  (let ((path (or file (emacs-srs-trainer-info-file))))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((table (make-hash-table :test 'equal))
              (line-number 0)
              (node "Top"))
          (goto-char (point-min))
          (while (not (eobp))
            (setq line-number (1+ line-number))
            (let ((line-text (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
              (when (string-match-p (rx bol "Tag Table:") line-text)
                (goto-char (point-max)))
              (when (string-match "^File:.*?,  Node: \\([^,\n]+\\)"
                                  line-text)
                (setq node (match-string 1 line-text)))
              (unless (eobp)
                (emacs-srs-trainer-info--scan-line
                 table line-text line-number node)))
            (forward-line 1))
          (sort (hash-table-values table)
                (lambda (a b)
                  (string< (plist-get a :key)
                           (plist-get b :key)))))))))

(defun emacs-srs-trainer-info-extraction-summary (&optional file)
  "Return a plist summarizing Info manual extraction for FILE."
  (let ((path (or file (emacs-srs-trainer-info-file))))
    (if (and path (file-readable-p path))
        (let ((candidates (emacs-srs-trainer-info-extract-keybindings path)))
          (list :ok t
                :path path
                :count (length candidates)
                :candidates candidates))
      (list :ok nil
            :path path
            :count 0
            :message "Info introduction manual not found"))))

(provide 'emacs-srs-trainer-info)

;;; emacs-srs-trainer-info.el ends here
