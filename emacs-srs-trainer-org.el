;;; emacs-srs-trainer-org.el --- Org manual discovery and extraction  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Locate the installed Org manual and extract important key candidates
;; from its Key Index.  The deck remains curated, while validation checks
;; that the important manual-wide command set is covered.

;;; Code:

(require 'cl-lib)
(require 'info)
(require 'subr-x)
(require 'emacs-srs-trainer-deck)

(defgroup emacs-srs-trainer-org nil
  "Installed Org manual extraction for `emacs-srs-trainer'."
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-org-extra-paths nil
  "Additional Org manual paths to try before Emacs's Info directories."
  :type '(repeat file)
  :group 'emacs-srs-trainer-org)

(defconst emacs-srs-trainer-org-ignored-keybindings
  '(("mouse-1" . "Mouse gesture; the deck trains keyboard-accessible equivalents.")
    ("mouse-2" . "Mouse gesture; the deck trains keyboard-accessible equivalents.")
    ("mouse-3" . "Mouse gesture; the deck trains keyboard-accessible equivalents."))
  "Extracted Org manual key candidates intentionally not used as cards.")

(defun emacs-srs-trainer-org--suffixes ()
  "Return file suffixes used when locating Info files."
  (mapcar (lambda (entry) (if (consp entry) (car entry) entry))
          Info-suffix-list))

(defun emacs-srs-trainer-org-candidate-paths ()
  "Return candidate paths for the installed Org manual."
  (let* ((dirs (delete-dups
                (delq nil
                      (append Info-directory-list
                              (when (fboundp 'Info-default-dirs)
                                (Info-default-dirs))))))
         (located (locate-file "org" dirs
                               (emacs-srs-trainer-org--suffixes))))
    (append emacs-srs-trainer-org-extra-paths
            (delq nil (list located)))))

(defun emacs-srs-trainer-org-file ()
  "Return the installed Org manual path, or nil."
  (cl-find-if #'file-readable-p
              (emacs-srs-trainer-org-candidate-paths)))

(defun emacs-srs-trainer-org--important-answer-table ()
  "Return a hash table of normalized answers from the curated Org deck."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (card emacs-srs-trainer-org-manual-cards table)
      (dolist (answer (emacs-srs-trainer-card-answers card))
        (when answer
          (condition-case nil
              (puthash (emacs-srs-trainer-normalize-key answer) t table)
            (error nil)))))))

(defun emacs-srs-trainer-org--clean-key-index-label (label)
  "Return a clean key label from an Org manual Key Index LABEL."
  (let ((clean (string-trim label)))
    (setq clean (replace-regexp-in-string (rx (+ space) "<" (+ digit) ">") "" clean))
    (setq clean (replace-regexp-in-string (rx (+ space) "(" (+? anything) ")" eos) "" clean))
    (setq clean (replace-regexp-in-string (rx bos "short" (+ space)) "" clean))
    (setq clean (replace-regexp-in-string "\\b\\(UP\\|DOWN\\|LEFT\\|RIGHT\\)\\b" "<\\1>" clean))
    (setq clean (replace-regexp-in-string "\\bTAB\\b" "TAB" clean))
    (string-trim clean)))

(defun emacs-srs-trainer-org--add-candidate
    (table key node line line-text)
  "Add KEY from NODE and LINE in LINE-TEXT to TABLE when valid."
  (condition-case nil
      (let* ((normalized (emacs-srs-trainer-normalize-key key))
             (existing (gethash normalized table))
             (source (format "ORG:%s:line-%d" node line)))
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

(defun emacs-srs-trainer-org--scan-key-index-line
    (table important line-text line-number)
  "Scan one Key Index LINE-TEXT at LINE-NUMBER into TABLE.

Only keys present in IMPORTANT are returned as coverage candidates."
  (when (string-match "^\\* \\(.+\\):[[:space:]]+\\(.+\\)$" line-text)
    (let* ((raw-key (match-string 1 line-text))
           (node-text (match-string 2 line-text))
           (key (emacs-srs-trainer-org--clean-key-index-label raw-key))
           (node (string-trim
                  (replace-regexp-in-string
                   (rx (+ space) "(" (+? anything) ")" (* space) eos)
                   ""
                   (replace-regexp-in-string
                    (rx "." (* space) eos) "" node-text)))))
      (condition-case nil
          (let ((normalized (emacs-srs-trainer-normalize-key key)))
            (when (gethash normalized important)
              (emacs-srs-trainer-org--add-candidate
               table key node line-number line-text)))
        (error nil)))))

(defun emacs-srs-trainer-org-extract-keybindings (&optional file)
  "Extract important keybinding candidates from the Org manual FILE.

Return a list of plists, one per unique normalized key.  If FILE is nil,
locate the installed Org manual.  Missing files return nil."
  (let ((path (or file (emacs-srs-trainer-org-file))))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((table (make-hash-table :test 'equal))
              (important (emacs-srs-trainer-org--important-answer-table))
              (line-number 0)
              (in-key-index nil))
          (goto-char (point-min))
          (while (not (eobp))
            (setq line-number (1+ line-number))
            (let ((line-text (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
              (cond
               ((string-match-p "^File: org\\.info,  Node: Key Index," line-text)
                (setq in-key-index t))
               ((and in-key-index
                     (string-match-p "^File: org\\.info,  Node: Command and Function Index," line-text))
                (setq in-key-index nil))
               (in-key-index
                (emacs-srs-trainer-org--scan-key-index-line
                 table important line-text line-number))))
            (forward-line 1))
          (sort (hash-table-values table)
                (lambda (a b)
                  (string< (plist-get a :key)
                           (plist-get b :key)))))))))

(defun emacs-srs-trainer-org-extraction-summary (&optional file)
  "Return a plist summarizing Org manual extraction for FILE."
  (let ((path (or file (emacs-srs-trainer-org-file))))
    (if (and path (file-readable-p path))
        (let ((candidates (emacs-srs-trainer-org-extract-keybindings path)))
          (list :ok t
                :path path
                :count (length candidates)
                :candidates candidates
                :ignored-count (length emacs-srs-trainer-org-ignored-keybindings)))
      (list :ok nil
            :path path
            :count 0
            :message "Org manual not found"))))

(provide 'emacs-srs-trainer-org)

;;; emacs-srs-trainer-org.el ends here
