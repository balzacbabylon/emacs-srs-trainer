;;; emacs-srs-trainer-validate.el --- Deck validation  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Validation for deck structure, key parsing, duplicate detection,
;; deterministic generation, source references, and tutorial coverage.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emacs-srs-trainer-deck)
(require 'emacs-srs-trainer-tutorial)
(require 'emacs-srs-trainer-info)

(defun emacs-srs-trainer-validate--ok-p (result)
  "Return non-nil when validation RESULT has no errors."
  (null (plist-get result :errors)))

(defun emacs-srs-trainer-validate--add-error (errors message &rest args)
  "Add formatted MESSAGE with ARGS to ERRORS."
  (cons (apply #'format message args) errors))

(defun emacs-srs-trainer-validate--source-ref-p (source-ref)
  "Return non-nil when SOURCE-REF is well formed."
  (and (stringp source-ref)
       (string-match-p (rx bos (or "TUTORIAL:" "INFO:") (+ not-newline) eos)
                       source-ref)))

(defun emacs-srs-trainer-validate--answer-parses-p (answer)
  "Return non-nil when ANSWER parses with `kbd'."
  (condition-case nil
      (progn
        (emacs-srs-trainer-normalize-key answer)
        t)
    (error nil)))

(defun emacs-srs-trainer-validate--hardware-specific-answer-p (answer)
  "Return non-nil when ANSWER depends on a non-universal hardware key."
  (condition-case nil
      (member (emacs-srs-trainer-normalize-key answer)
              '("<next>" "<prior>" "<pagedown>" "<pageup>"
                "<PageDown>" "<PageUp>"))
    (error nil)))

(defun emacs-srs-trainer-validate--plain-printable-answer-p (answer)
  "Return non-nil when ANSWER is just one unmodified printable character."
  (condition-case nil
      (let ((normalized (emacs-srs-trainer-normalize-key answer)))
        (and (= (length normalized) 1)
             (string-match-p (rx graph) normalized)))
    (error nil)))

(defun emacs-srs-trainer-validate--del-answer-p (answer)
  "Return non-nil when ANSWER uses Emacs DEL notation."
  (condition-case nil
      (member (emacs-srs-trainer-normalize-key answer) '("DEL" "M-DEL"))
    (error nil)))

(defun emacs-srs-trainer-validate--plain-printable-key-description-p
    (description)
  "Return non-nil when DESCRIPTION is one unmodified printable character."
  (and (stringp description)
       (= (length description) 1)
       (string-match-p (rx graph) description)))

(defun emacs-srs-trainer-validate--question-contains-p (question phrase)
  "Return non-nil when QUESTION contains PHRASE case-insensitively."
  (and (stringp question)
       (string-match-p (regexp-quote (downcase phrase))
                       (downcase question))))

(defun emacs-srs-trainer-validate--question-leak-phrases-for-token (token)
  "Return phrases that would leak answer TOKEN in a question."
  (let ((lower-token (downcase token)))
    (append
     (when (or (string-match-p (rx bos (or "C-" "M-" "C-M-")) token)
               (string-match-p (rx bos "<" (+ anything) ">" eos) token)
               (member token '("DEL" "RET" "TAB" "SPC" "ESC")))
       (list lower-token))
     (cond
      ((string-match-p (rx bos "C-M-") token)
       '("control-meta" "control meta" "ctrl-meta" "meta" "alt" "control" "ctrl"))
      ((string-match-p (rx bos "C-") token)
       '("control" "ctrl"))
      ((string-match-p (rx bos "M-") token)
       '("meta" "alt"))
      (t nil))
     (cond
      ((member token '("DEL" "M-DEL" "<backspace>" "M-<backspace>"
                       "<delete>" "M-<delete>"))
       '("del" "backspace" "delete/backspace" "delete key" "mac delete"))
      ((string= token "RET")
       '("ret" "return key" "enter key"))
      ((string= token "TAB")
       '("tab" "tab key"))
      ((string= token "ESC")
       '("esc" "escape"))
      (t nil)))))

(defun emacs-srs-trainer-validate--answer-leakage-errors (card)
  "Return validation errors when CARD's question leaks its answer."
  (let* ((id (plist-get card :id))
         (question (plist-get card :question))
         (answers (delq nil (emacs-srs-trainer-card-answers card)))
         (leaks nil)
         (phrases nil))
    (dolist (answer answers)
      (condition-case nil
          (let* ((normalized (emacs-srs-trainer-normalize-key answer))
                 (tokens (split-string normalized " " t)))
            (if (emacs-srs-trainer-validate--plain-printable-key-description-p
                 normalized)
                (let ((lower-normalized (downcase normalized)))
                  (setq phrases
                        (append (list (format "%s key" lower-normalized)
                                      (format "letter %s" lower-normalized)
                                      (format "type %s" lower-normalized)
                                      (format "press %s" lower-normalized))
                                phrases)))
              (push normalized phrases))
            (dolist (token tokens)
              (setq phrases
                    (append (emacs-srs-trainer-validate--question-leak-phrases-for-token
                             token)
                            phrases))))
        (error nil)))
    (dolist (phrase (delete-dups (delq nil phrases)))
      (when (and (not (string-empty-p phrase))
                 (emacs-srs-trainer-validate--question-contains-p
                  question phrase))
        (push phrase leaks)))
    (when leaks
      (list (format "Card %S question leaks the answer via: %s"
                    id
                    (string-join (sort leaks #'string<) ", "))))))

(defun emacs-srs-trainer-validate--trivial-card-error (card)
  "Return a validation error when CARD looks trivial or machine-specific."
  (let ((id (plist-get card :id))
        (command (plist-get card :command))
        (canonical (plist-get card :canonical-answer))
        (answers (emacs-srs-trainer-card-answers card))
        (tags (plist-get card :tags))
        (display-answer (plist-get card :display-answer))
        (metadata (plist-get card :metadata)))
    (cond
     ((cl-some #'emacs-srs-trainer-validate--hardware-specific-answer-p answers)
      (format "Card %S uses a hardware-specific PageUp/PageDown style answer: %S"
              id canonical))
     ((and (emacs-srs-trainer-validate--del-answer-p canonical)
           (not (and (stringp display-answer)
                     (string-match-p "Backspace" display-answer))))
      (format "Card %S exposes DEL notation without a Backspace/Delete display answer"
              id))
     ((member "prompt" tags)
      (format "Card %S trains an incidental prompt response; remove it or model the underlying command"
              id))
     ((and (eq command 'self-insert-command)
           (not (equal (plist-get metadata :variation) "numeric-prefix")))
      (format "Card %S trains ordinary self-insertion rather than a meaningful command variation"
              id))
     ((and (null command)
           (emacs-srs-trainer-validate--plain-printable-answer-p canonical))
      (format "Card %S has no command and only asks for a plain printable character: %S"
              id canonical)))))

(defun emacs-srs-trainer-validate--answer-set (cards)
  "Return hash table of normalized answers covered by CARDS."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (card cards)
      (dolist (answer (emacs-srs-trainer-card-answers card))
        (when answer
          (condition-case nil
              (puthash (emacs-srs-trainer-normalize-key answer) t table)
            (error nil)))))
    table))

(defun emacs-srs-trainer-validate--ignored-answer-table (entries)
  "Return a hash table of ignored normalized key candidates from ENTRIES."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries table)
      (let ((key (car entry)))
        (condition-case nil
            (puthash (emacs-srs-trainer-normalize-key key) (cdr entry) table)
          (error (puthash key (cdr entry) table)))))))

(defun emacs-srs-trainer-validate--coverage-errors
    (label candidates ignored-entries answer-set)
  "Return coverage errors for LABEL CANDIDATES.

IGNORED-ENTRIES is an alist of intentionally ignored keys.  ANSWER-SET
is the normalized answer hash table for all loaded cards."
  (let ((ignored (emacs-srs-trainer-validate--ignored-answer-table
                  ignored-entries))
        (errors nil))
    (dolist (candidate candidates)
      (let ((key (plist-get candidate :key)))
        (unless (or (gethash key answer-set)
                    (gethash key ignored))
          (push (format "Extracted %s key is not covered or ignored: %s (%s)"
                        label
                        key
                        (plist-get candidate :source-ref))
                errors))))
    (nreverse errors)))

(defun emacs-srs-trainer-validate-deck-data (&optional cards)
  "Validate CARDS and return a result plist.

When CARDS is nil, validate all loaded cards."
  (let* ((deck-cards (or cards (emacs-srs-trainer-all-cards)))
         (errors nil)
         (warnings nil)
         (ids (make-hash-table :test 'equal))
         (question-answer-pairs (make-hash-table :test 'equal)))
    (dolist (card deck-cards)
      (let ((id (plist-get card :id)))
        (dolist (field emacs-srs-trainer-required-card-fields)
          (unless (plist-member card field)
            (setq errors (emacs-srs-trainer-validate--add-error
                          errors "Card %S is missing required field %S" id field))))
        (unless (and (stringp id) (not (string-empty-p id)))
          (setq errors (emacs-srs-trainer-validate--add-error
                        errors "Card has invalid id: %S" id)))
        (when (and id (gethash id ids))
          (setq errors (emacs-srs-trainer-validate--add-error
                        errors "Duplicate card id: %s" id)))
        (when id
          (puthash id t ids))
        (let ((question (plist-get card :question))
              (canonical (plist-get card :canonical-answer))
              (source-ref (plist-get card :source-ref)))
          (unless (and (stringp question) (not (string-empty-p question)))
            (setq errors (emacs-srs-trainer-validate--add-error
                          errors "Card %S has an empty question" id)))
          (unless (and (stringp canonical) (not (string-empty-p canonical)))
            (setq errors (emacs-srs-trainer-validate--add-error
                          errors "Card %S has an empty canonical answer" id)))
          (when (and (stringp canonical)
                     (not (emacs-srs-trainer-validate--answer-parses-p canonical)))
            (setq errors (emacs-srs-trainer-validate--add-error
                          errors "Card %S canonical answer does not parse with kbd: %S"
                          id canonical)))
          (dolist (accepted (plist-get card :accepted-answers))
            (unless (emacs-srs-trainer-validate--answer-parses-p accepted)
              (setq errors (emacs-srs-trainer-validate--add-error
                            errors "Card %S accepted answer does not parse with kbd: %S"
                            id accepted))))
          (when (and question canonical)
            (let ((pair (format "%s\0%s"
                                question
                                (condition-case nil
                                    (emacs-srs-trainer-normalize-key canonical)
                                  (error canonical)))))
              (when (gethash pair question-answer-pairs)
                (setq errors (emacs-srs-trainer-validate--add-error
                              errors "Duplicate question/answer pair: %s / %s"
                              question canonical)))
              (puthash pair t question-answer-pairs)))
          (unless (emacs-srs-trainer-validate--source-ref-p source-ref)
            (setq errors (emacs-srs-trainer-validate--add-error
                          errors "Card %S has malformed source-ref: %S"
                          id source-ref)))
          (when-let* ((trivial-error
                       (emacs-srs-trainer-validate--trivial-card-error card)))
            (setq errors (cons trivial-error errors)))
          (dolist (leak-error
                   (emacs-srs-trainer-validate--answer-leakage-errors card))
            (setq errors (cons leak-error errors))))))
    (let ((generated-a (emacs-srs-trainer-deck-generate-prefix-cards))
          (generated-b (emacs-srs-trainer-deck-generate-prefix-cards)))
      (unless (equal generated-a generated-b)
        (setq errors (emacs-srs-trainer-validate--add-error
                      errors "Generated prefix cards are not deterministic"))))
    (let* ((extract-summary (emacs-srs-trainer-tutorial-extraction-summary))
           (tutorial-candidates
            (when (plist-get extract-summary :ok)
              (emacs-srs-trainer-tutorial-extract-keybindings
               (plist-get extract-summary :path))))
           (info-summary (emacs-srs-trainer-info-extraction-summary))
           (info-candidates (when (plist-get info-summary :ok)
                              (plist-get info-summary :candidates)))
           (answer-set (emacs-srs-trainer-validate--answer-set deck-cards)))
      (if (not (plist-get extract-summary :ok))
          (setq errors (emacs-srs-trainer-validate--add-error
                        errors "Tutorial file unavailable: %s"
                        (or (plist-get extract-summary :message) "unknown error")))
        (dolist (coverage-error
                 (emacs-srs-trainer-validate--coverage-errors
                  "tutorial"
                  (sort tutorial-candidates
                        (lambda (a b)
                          (string< (plist-get a :key)
                                   (plist-get b :key))))
                  emacs-srs-trainer-tutorial-ignored-keybindings
                  answer-set))
          (setq errors (cons coverage-error errors))))
      (if (not (plist-get info-summary :ok))
          (push (format "Info introduction manual unavailable; skipping Info coverage extraction: %s"
                        (or (plist-get info-summary :message) "unknown error"))
                warnings)
        (dolist (coverage-error
                 (emacs-srs-trainer-validate--coverage-errors
                  "Info"
                  info-candidates
                  emacs-srs-trainer-info-ignored-keybindings
                  answer-set))
          (setq errors (cons coverage-error errors)))
        (setq info-summary
              (plist-put info-summary
                         :ignored-count
                         (length emacs-srs-trainer-info-ignored-keybindings))))
      (list :ok (null errors)
            :errors (nreverse errors)
            :warnings (nreverse warnings)
            :card-count (length deck-cards)
            :extracted-count (length tutorial-candidates)
            :ignored-count (length emacs-srs-trainer-tutorial-ignored-keybindings)
            :info-extraction info-summary))))

(defun emacs-srs-trainer-validate-format-result (result)
  "Format validation RESULT for display."
  (concat
   (format "Deck validation: %s\n" (if (plist-get result :ok) "ok" "failed"))
   (format "Cards: %d\n" (or (plist-get result :card-count) 0))
   (format "Extracted tutorial keys: %d\n" (or (plist-get result :extracted-count) 0))
   (format "Explicitly ignored extracted keys: %d\n"
           (or (plist-get result :ignored-count) 0))
   (when-let* ((info-result (plist-get result :info-extraction)))
     (concat
      (format "Extracted Info keys: %d\n"
              (or (plist-get info-result :count) 0))
      (format "Explicitly ignored extracted Info keys: %d\n"
              (or (plist-get info-result :ignored-count) 0))))
   (when (plist-get result :warnings)
     (concat "\nWarnings:\n"
             (mapconcat (lambda (warning) (concat "- " warning))
                        (plist-get result :warnings)
                        "\n")
             "\n"))
   (when (plist-get result :errors)
     (concat "\nErrors:\n"
             (mapconcat (lambda (error) (concat "- " error))
                        (plist-get result :errors)
                        "\n")
             "\n"))))

;;;###autoload
(defun emacs-srs-trainer-validate-deck ()
  "Validate loaded decks and tutorial coverage."
  (interactive)
  (let* ((result (emacs-srs-trainer-validate-deck-data))
         (text (emacs-srs-trainer-validate-format-result result)))
    (if noninteractive
        (princ text)
      (with-current-buffer (get-buffer-create "*Emacs SRS Trainer Validation*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (goto-char (point-min))
          (view-mode 1))
        (pop-to-buffer (current-buffer))))
    (unless (plist-get result :ok)
      (when noninteractive
        (kill-emacs 1)))
    result))

(provide 'emacs-srs-trainer-validate)

;;; emacs-srs-trainer-validate.el ends here
