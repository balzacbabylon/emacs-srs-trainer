;;; emacs-srs-trainer-test.el --- Tests for emacs-srs-trainer  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run with:
;; emacs -Q --batch -L . -l emacs-srs-trainer-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-srs-trainer)

(defun emacs-srs-trainer-test--card (id)
  "Return test card by ID."
  (cl-find id (emacs-srs-trainer-all-cards)
           :key #'emacs-srs-trainer-card-id
           :test #'string=))

(defun emacs-srs-trainer-test--synthetic-card (answer &optional accepted)
  "Return a minimal test card with canonical ANSWER and ACCEPTED alternatives."
  (list :id "synthetic"
        :deck "Synthetic"
        :topic "Synthetic"
        :question "Synthetic question"
        :canonical-answer answer
        :accepted-answers accepted
        :tags nil
        :source-ref "TUTORIAL:test"))

(defun emacs-srs-trainer-test--synthetic-deck-card
    (id deck answer &optional accepted)
  "Return a synthetic card with ID in DECK for review-loop tests."
  (let ((card (emacs-srs-trainer-test--synthetic-card answer accepted)))
    (setq card (plist-put card :id id))
    (setq card (plist-put card :deck deck))
    card))

(ert-deftest emacs-srs-trainer-test-deck-loading ()
  (should (assoc emacs-srs-trainer-tutorial-deck-name
                 (emacs-srs-trainer-load-decks)))
  (should (assoc emacs-srs-trainer-info-deck-name
                 (emacs-srs-trainer-load-decks)))
  (should (assoc emacs-srs-trainer-org-deck-name
                 (emacs-srs-trainer-load-decks)))
  (should (> (length (emacs-srs-trainer-all-cards)) 200)))

(ert-deftest emacs-srs-trainer-test-info-deck-loading ()
  (let ((cards (emacs-srs-trainer-deck-by-name
                emacs-srs-trainer-info-deck-name)))
    (should (> (length cards) 40))
    (should (emacs-srs-trainer-test--card "info-search-text"))
    (should (emacs-srs-trainer-test--card "info-quoted-insert"))
    (should (emacs-srs-trainer-test--card "info-numbered-info-buffer"))
    (should (cl-every
             (lambda (card)
               (string-prefix-p "INFO:" (plist-get card :source-ref)))
             cards))))

(ert-deftest emacs-srs-trainer-test-org-deck-loading ()
  (let ((cards (emacs-srs-trainer-deck-by-name
                emacs-srs-trainer-org-deck-name)))
    (should (> (length cards) 150))
    (should (emacs-srs-trainer-test--card "org-cycle-visibility"))
    (should (emacs-srs-trainer-test--card "org-agenda"))
    (should (emacs-srs-trainer-test--card "org-export-dispatch"))
    (should (emacs-srs-trainer-test--card "org-babel-tangle"))
    (should (cl-every
             (lambda (card)
               (string-prefix-p "ORG:" (plist-get card :source-ref)))
             cards))))

(ert-deftest emacs-srs-trainer-test-required-deck-fields ()
  (let ((result (emacs-srs-trainer-validate-deck-data)))
    (should (plist-get result :ok))))

(ert-deftest emacs-srs-trainer-test-duplicate-card-detection ()
  (let* ((card (copy-sequence (car (emacs-srs-trainer-all-cards))))
         (duplicate (copy-sequence card))
         (result (emacs-srs-trainer-validate-deck-data
                  (list card duplicate))))
    (should (cl-some (lambda (error)
                       (string-match-p "Duplicate card id" error))
                     (plist-get result :errors)))))

(ert-deftest emacs-srs-trainer-test-trivial-card-validation ()
  (let* ((prompt-card (list :id "prompt-card"
                            :deck "Synthetic"
                            :topic "Synthetic"
                            :command nil
                            :question "Decline a prompt."
                            :canonical-answer "n"
                            :accepted-answers nil
                            :tags '("tutorial" "prompt")
                            :source-ref "TUTORIAL:test"))
         (hardware-card (list :id "hardware-card"
                              :deck "Synthetic"
                              :topic "Synthetic"
                              :command 'scroll-up-command
                              :question "Use PageDown."
                              :canonical-answer "<next>"
                              :accepted-answers nil
                              :tags '("tutorial" "viewing")
                              :source-ref "TUTORIAL:test"))
         (literal-card (list :id "literal-card"
                             :deck "Synthetic"
                             :topic "Synthetic"
                             :command 'self-insert-command
                             :question "Insert an asterisk."
                             :canonical-answer "*"
                             :accepted-answers nil
                             :tags '("tutorial" "insertion")
                             :source-ref "TUTORIAL:test"))
         (naked-del-card (list :id "naked-del-card"
                               :deck "Synthetic"
                               :topic "Synthetic"
                               :command 'delete-backward-char
                               :question "Delete backward."
                               :canonical-answer "DEL"
                               :accepted-answers nil
                               :tags '("tutorial" "deletion")
                               :source-ref "TUTORIAL:test"))
         (result (emacs-srs-trainer-validate-deck-data
                  (list prompt-card hardware-card literal-card naked-del-card)))
         (errors (plist-get result :errors)))
    (should (cl-some (lambda (error)
                       (string-match-p "incidental prompt response" error))
                     errors))
    (should (cl-some (lambda (error)
                       (string-match-p "hardware-specific PageUp/PageDown" error))
                     errors))
    (should (cl-some (lambda (error)
                       (string-match-p "ordinary self-insertion" error))
                     errors))
    (should (cl-some (lambda (error)
                       (string-match-p "without a Backspace/Delete display answer"
                                       error))
                     errors))))

(ert-deftest emacs-srs-trainer-test-question-answer-leakage-validation ()
  (let* ((exact-card (list :id "exact-leak"
                           :deck "Synthetic"
                           :topic "Synthetic"
                           :command 'find-file
                           :question "Use C-x C-f to visit a file."
                           :canonical-answer "C-x C-f"
                           :accepted-answers nil
                           :tags '("tutorial")
                           :source-ref "TUTORIAL:test"))
         (modifier-card (list :id "modifier-leak"
                              :deck "Synthetic"
                              :topic "Synthetic"
                              :command 'backward-kill-word
                              :question "Kill the word before point using Meta plus Backspace."
                              :canonical-answer "M-DEL"
                              :accepted-answers nil
                              :tags '("tutorial")
                              :source-ref "TUTORIAL:test"
                              :display-answer "M-Backspace/Delete (M-DEL)"))
         (clean-card (list :id "clean-card"
                           :deck "Synthetic"
                           :topic "Synthetic"
                           :command 'backward-kill-word
                           :question "Kill the word immediately before point."
                           :canonical-answer "M-DEL"
                           :accepted-answers nil
                           :tags '("tutorial")
                           :source-ref "TUTORIAL:test"
                           :display-answer "M-Backspace/Delete (M-DEL)"))
         (result (emacs-srs-trainer-validate-deck-data
                  (list exact-card modifier-card clean-card)))
         (errors (plist-get result :errors)))
    (should (cl-some (lambda (error)
                       (and (string-match-p "exact-leak" error)
                            (string-match-p "question leaks" error)))
                     errors))
    (should (cl-some (lambda (error)
                       (and (string-match-p "modifier-leak" error)
                            (string-match-p "question leaks" error)))
                     errors))
    (should-not (cl-some (lambda (error)
                           (string-match-p "clean-card" error))
                         errors))))

(ert-deftest emacs-srs-trainer-test-single-letter-card-leakage-validation ()
  (let* ((clean-card (list :id "clean-single-letter"
                           :deck "Synthetic"
                           :topic "Synthetic"
                           :command 'Info-next
                           :question "Go to the following node at the same level."
                           :canonical-answer "n"
                           :accepted-answers nil
                           :tags '("info")
                           :source-ref "INFO:test"))
         (leaky-card (list :id "leaky-single-letter"
                           :deck "Synthetic"
                           :topic "Synthetic"
                           :command 'Info-next
                           :question "Press n key for the following node."
                           :canonical-answer "n"
                           :accepted-answers nil
                           :tags '("info")
                           :source-ref "INFO:test"))
         (errors (plist-get (emacs-srs-trainer-validate-deck-data
                             (list clean-card leaky-card))
                            :errors)))
    (should-not (cl-some (lambda (error)
                           (string-match-p "clean-single-letter" error))
                         errors))
    (should (cl-some (lambda (error)
                       (and (string-match-p "leaky-single-letter" error)
                            (string-match-p "question leaks" error)))
                     errors))))

(ert-deftest emacs-srs-trainer-test-key-notation-parsing ()
  (dolist (key '("C-f" "M-f" "C-x C-f" "C-u 8 C-f" "C-M-v" "ESC ESC ESC"))
    (should (key-description (kbd key)))
    (should (stringp (emacs-srs-trainer-normalize-key key)))))

(ert-deftest emacs-srs-trainer-test-key-normalization ()
  (should (string= (emacs-srs-trainer-normalize-key (kbd "C-f")) "C-f"))
  (should (string= (emacs-srs-trainer-normalize-key "<ESC> f") "M-f"))
  (should (string= (emacs-srs-trainer-normalize-key "C-<SPC>") "C-SPC"))
  (should (string= (emacs-srs-trainer-normalize-key "<Return>") "RET"))
  (should (string= (emacs-srs-trainer-normalize-key [return]) "RET"))
  (should (string= (emacs-srs-trainer-normalize-key [M-return]) "M-RET"))
  (should (string= (emacs-srs-trainer-normalize-key [C-return]) "C-RET"))
  (should (string= (emacs-srs-trainer-normalize-key [3 return]) "C-c RET"))
  (should (string= (emacs-srs-trainer-normalize-key "<RIGHT>") "<right>"))
  (should (string= (emacs-srs-trainer-normalize-key "<right>") "<right>"))
  (should (string= (emacs-srs-trainer-normalize-key "M-<RIGHT>") "M-<right>"))
  (should (string= (emacs-srs-trainer-normalize-key "M-<right>") "M-<right>")))

(ert-deftest emacs-srs-trainer-test-key-display-compacts-typed-text ()
  (should (string= (emacs-srs-trainer-display-key "M-x replace-string RET")
                   "M-x replace-string RET"))
  (should (string= (emacs-srs-trainer-display-key "C-u 2 0 C-x f")
                   "C-u 20 C-x f"))
  (should (string= (emacs-srs-trainer-display-key "g * RET")
                   "g * RET")))

(ert-deftest emacs-srs-trainer-test-org-arrow-key-grading ()
  (let* ((card (emacs-srs-trainer-test--card "org-do-demote"))
         (grade (emacs-srs-trainer-grade-answer card "M-<right>")))
    (should (plist-get grade :correct))
    (should (string= (plist-get grade :answer) "M-<right>"))))

(ert-deftest emacs-srs-trainer-test-gui-return-key-grading ()
  (dolist (case '(("tutorial-finish-prompt" [return] "RET")
                  ("org-meta-return" [M-return] "M-RET")
                  ("org-insert-heading-respect-content" [C-return] "C-RET")
                  ("org-table-hline-and-move" [3 return] "C-c RET")))
    (pcase-let ((`(,card-id ,answer ,normalized-answer) case))
      (let ((grade (emacs-srs-trainer-grade-answer
                    (emacs-srs-trainer-test--card card-id)
                    answer)))
        (should (plist-get grade :correct))
        (should (string= (plist-get grade :answer) normalized-answer))))))

(ert-deftest emacs-srs-trainer-test-correct-answer-grading ()
  (let* ((card (emacs-srs-trainer-test--card "tutorial-move-forward-char"))
         (grade (emacs-srs-trainer-grade-answer card "C-f")))
    (should (plist-get grade :correct))
    (should (string= (plist-get grade :answer) "C-f"))))

(ert-deftest emacs-srs-trainer-test-accepted-alternative-grading ()
  (let* ((card (emacs-srs-trainer-test--card "tutorial-forward-word"))
         (grade (emacs-srs-trainer-grade-answer card "<ESC> f")))
    (should (plist-get grade :correct))
    (should (string= (plist-get grade :answer) "M-f"))))

(ert-deftest emacs-srs-trainer-test-extended-command-card-grading ()
  (dolist (case '(("tutorial-replace-string" "M-x replace-string RET")
                  ("info-apropos" "M-x info-apropos RET")
                  ("org-version" "M-x org-version RET")
                  ("org-reload" "C-u M-x org-reload RET")))
    (pcase-let ((`(,card-id ,answer) case))
      (let* ((card (emacs-srs-trainer-test--card card-id))
             (correct-grade (emacs-srs-trainer-grade-answer card answer))
             (precursor-grade (emacs-srs-trainer-grade-answer card "M-x")))
        (should (plist-get correct-grade :correct))
        (should-not (plist-get precursor-grade :correct))
        (should (string= (emacs-srs-trainer-card-display-answer card)
                         answer))))))

(ert-deftest emacs-srs-trainer-test-backspace-alternative-grading ()
  (let* ((card (emacs-srs-trainer-test--card "tutorial-delete-backward-char"))
         (grade (emacs-srs-trainer-grade-answer card "<backspace>")))
    (should (plist-get grade :correct))
    (should (string-match-p "Backspace/Delete"
                            (emacs-srs-trainer-card-display-answer card)))
    (should (string-match-p "Backspace/Delete"
                            (emacs-srs-trainer-display-key "DEL")))))

(ert-deftest emacs-srs-trainer-test-incorrect-answer-grading ()
  (let* ((card (emacs-srs-trainer-test--card "tutorial-move-forward-char"))
         (grade (emacs-srs-trainer-grade-answer card "C-b")))
    (should-not (plist-get grade :correct))
    (should (string= (plist-get grade :answer) "C-b"))))

(ert-deftest emacs-srs-trainer-test-scheduler-correct ()
  (let* ((now 1000.0)
         (state (emacs-srs-trainer-scheduler-new-state now))
         (updated (emacs-srs-trainer-scheduler-review state t now)))
    (should (= (plist-get updated :repetition-count) 1))
    (should (= (plist-get updated :learning-step) 1))
    (should (= (plist-get updated :interval) 0))
    (should (eq (plist-get updated :last-result) 'correct))
    (should (= (plist-get updated :due) (+ now 600)))))

(ert-deftest emacs-srs-trainer-test-scheduler-learning-steps ()
  (let* ((now 1000.0)
         (new-state (emacs-srs-trainer-scheduler-new-state now))
         (after-first-correct
          (emacs-srs-trainer-scheduler-review new-state t now))
         (after-second-correct
          (emacs-srs-trainer-scheduler-review
           after-first-correct t (+ now 600)))
         (after-third-correct
          (emacs-srs-trainer-scheduler-review
           after-second-correct t (+ now 600 86400))))
    (should (= (plist-get after-first-correct :learning-step) 1))
    (should (= (plist-get after-first-correct :due) (+ now 600)))
    (should (= (plist-get after-second-correct :learning-step) 2))
    (should (= (plist-get after-second-correct :due)
               (+ now 600 86400)))
    (should (plist-get after-third-correct :graduated))
    (should (eq (emacs-srs-trainer-scheduler-card-type after-third-correct)
                'review))
    (should (= (plist-get after-third-correct :interval)
               emacs-srs-trainer-scheduler-graduating-interval-days))))

(ert-deftest emacs-srs-trainer-test-scheduler-incorrect ()
  (let* ((now 1000.0)
         (state (emacs-srs-trainer-scheduler-new-state now))
         (updated (emacs-srs-trainer-scheduler-review state nil now)))
    (should (= (car emacs-srs-trainer-scheduler-learning-steps-seconds) 60))
    (should (= (plist-get updated :repetition-count) 0))
    (should (= (plist-get updated :learning-step) 0))
    (should (= (plist-get updated :lapse-count) 1))
    (should (eq (plist-get updated :last-result) 'incorrect))
    (should (= (plist-get updated :due)
               (+ now 60)))))

(ert-deftest emacs-srs-trainer-test-scheduler-card-types ()
  (let* ((now 1000.0)
         (new-state (emacs-srs-trainer-scheduler-new-state now))
         (learning-after-wrong
          (emacs-srs-trainer-scheduler-review new-state nil now))
         (learning-after-first-correct
          (emacs-srs-trainer-scheduler-review new-state t now))
         (still-learning-after-second-correct
          (emacs-srs-trainer-scheduler-review
           learning-after-first-correct t now))
         (review-after-third-correct
          (emacs-srs-trainer-scheduler-review
           still-learning-after-second-correct t now)))
    (should (eq (emacs-srs-trainer-scheduler-card-type new-state) 'new))
    (should (eq (emacs-srs-trainer-scheduler-card-type learning-after-wrong)
                'learning))
    (should (eq (emacs-srs-trainer-scheduler-card-type
                 learning-after-first-correct)
                'learning))
    (should (eq (emacs-srs-trainer-scheduler-card-type
                 still-learning-after-second-correct)
                'learning))
    (should (eq (emacs-srs-trainer-scheduler-card-type
                 review-after-third-correct)
                'review))
    (should (string= (emacs-srs-trainer-scheduler-card-type-label 'review)
                     "To Review"))))

(ert-deftest emacs-srs-trainer-test-queue-counts ()
  (let* ((now 1000.0)
         (cards (cl-subseq (emacs-srs-trainer-all-cards) 0 3))
         (learning-state
          (emacs-srs-trainer-scheduler-review
           (emacs-srs-trainer-scheduler-new-state now) nil now))
         (review-state
          (emacs-srs-trainer-scheduler-review
           (emacs-srs-trainer-scheduler-review
            (emacs-srs-trainer-scheduler-review
             (emacs-srs-trainer-scheduler-new-state now) t now)
            t now)
           t now))
         (storage (emacs-srs-trainer-storage-empty-state)))
    (setq storage (emacs-srs-trainer-storage-put-card-state
                   (emacs-srs-trainer-card-id (nth 1 cards))
                   learning-state storage))
    (setq storage (emacs-srs-trainer-storage-put-card-state
                   (emacs-srs-trainer-card-id (nth 2 cards))
                   review-state storage))
    (let ((counts (emacs-srs-trainer--queue-counts-for-cards
                   cards storage now)))
      (should (= (plist-get counts :new) 1))
      (should (= (plist-get counts :learning) 1))
      (should (= (plist-get counts :review) 1))
      (should (string= (emacs-srs-trainer-format-queue-counts counts)
                       "New: 1    Learning: 1    To Review: 1")))))

(ert-deftest emacs-srs-trainer-test-shuffles-cards-within-queue ()
  (let* ((now 1000.0)
         (cards (cl-subseq (emacs-srs-trainer-all-cards) 0 5))
         (state (emacs-srs-trainer-storage-empty-state))
         (ids (mapcar #'emacs-srs-trainer-card-id cards)))
    (let ((emacs-srs-trainer-shuffle-cards-within-queue nil))
      (should (equal (mapcar #'emacs-srs-trainer-card-id
                             (emacs-srs-trainer--sort-cards-by-queue
                              cards state now))
                     ids)))
    (let ((emacs-srs-trainer-shuffle-cards-within-queue t))
      (cl-letf (((symbol-function 'random) (lambda (_limit) 0)))
        (let ((shuffled (mapcar #'emacs-srs-trainer-card-id
                                (emacs-srs-trainer--sort-cards-by-queue
                                 cards state now))))
          (should-not (equal shuffled ids))
          (should (equal (sort (copy-sequence shuffled) #'string<)
                         (sort (copy-sequence ids) #'string<))))))))

(ert-deftest emacs-srs-trainer-test-shuffle-preserves-queue-buckets ()
  (let* ((now 1000.0)
         (cards (cl-subseq (emacs-srs-trainer-all-cards) 0 3))
         (learning-card (nth 2 cards))
         (state (emacs-srs-trainer-storage-put-card-state
                 (emacs-srs-trainer-card-id learning-card)
                 (emacs-srs-trainer-scheduler-review
                  (emacs-srs-trainer-scheduler-new-state now) nil now)
                 (emacs-srs-trainer-storage-empty-state)))
         (emacs-srs-trainer-shuffle-cards-within-queue t))
    (cl-letf (((symbol-function 'random) (lambda (_limit) 0)))
      (should (string= (emacs-srs-trainer-card-id
                        (car (emacs-srs-trainer--sort-cards-by-queue
                              cards state now)))
                       (emacs-srs-trainer-card-id learning-card))))))

(ert-deftest emacs-srs-trainer-test-new-cards-are-due ()
  (let* ((now 1000.0)
         (state (emacs-srs-trainer-storage-empty-state))
         (due (emacs-srs-trainer-due-cards nil nil now state)))
    (should (= (length due)
               (length (emacs-srs-trainer-deck-by-name
                        emacs-srs-trainer-default-deck))))))

(ert-deftest emacs-srs-trainer-test-persistence-save-load ()
  (let* ((dir (make-temp-file "emacs-srs-trainer-test-" t))
         (file (expand-file-name "state.el" dir))
         (state (emacs-srs-trainer-storage-put-card-state
                 "tutorial-move-forward-char"
                 (emacs-srs-trainer-scheduler-new-state 1000.0)
                 (emacs-srs-trainer-storage-empty-state))))
    (unwind-protect
        (progn
          (emacs-srs-trainer-storage-save state file)
          (let ((loaded (emacs-srs-trainer-storage-load file)))
            (should (equal state loaded))
            (should (assoc "tutorial-move-forward-char"
                           (plist-get loaded :cards)))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-tutorial-keybinding-extraction ()
  (unless (emacs-srs-trainer-tutorial-file)
    (ert-skip "Installed tutorial file is unavailable"))
  (let ((keys (mapcar (lambda (candidate)
                        (plist-get candidate :key))
                      (emacs-srs-trainer-tutorial-extract-keybindings))))
    (dolist (key '("C-f" "M-f" "C-x C-f" "C-u 8 C-f" "C-g"))
      (should (member key keys)))))

(ert-deftest emacs-srs-trainer-test-info-keybinding-extraction ()
  (unless (emacs-srs-trainer-info-file)
    (ert-skip "Installed Info introduction manual is unavailable"))
  (let ((keys (mapcar (lambda (candidate)
                        (plist-get candidate :key))
                      (emacs-srs-trainer-info-extract-keybindings))))
    (dolist (key '("h" "?" "n" "SPC" "DEL" "m" "f" "l" "s" "i"
                   "C-q" "C-q ?" "C-s" "C-r" "g" "g * RET" "M-n"
                   "C-u m" "C-u 2 C-h i"))
      (should (member key keys)))))

(ert-deftest emacs-srs-trainer-test-org-keybinding-extraction ()
  (unless (emacs-srs-trainer-org-file)
    (ert-skip "Installed Org manual is unavailable"))
  (let ((keys (mapcar (lambda (candidate)
                        (plist-get candidate :key))
                      (emacs-srs-trainer-org-extract-keybindings))))
    (dolist (key '("TAB" "C-c C-t" "C-c C-e" "C-c C-e h h"
                   "C-c C-v t" "C-c C-x TAB" "C-c C-a"
                   "C-c C-w"))
      (should (member key keys)))))

(ert-deftest emacs-srs-trainer-test-tutorial-coverage-validation ()
  (unless (emacs-srs-trainer-tutorial-file)
    (ert-skip "Installed tutorial file is unavailable"))
  (let ((result (emacs-srs-trainer-validate-deck-data)))
    (should (plist-get result :ok))))

(ert-deftest emacs-srs-trainer-test-info-deck-selection ()
  (let ((cards (emacs-srs-trainer-due-cards
                t nil nil nil emacs-srs-trainer-info-deck-name)))
    (should (= (length cards)
               (length (emacs-srs-trainer-deck-by-name
                        emacs-srs-trainer-info-deck-name))))
    (should (cl-find "info-search-text" cards
                     :key #'emacs-srs-trainer-card-id
                     :test #'string=))
    (should-not (cl-find "tutorial-move-forward-char" cards
                         :key #'emacs-srs-trainer-card-id
                         :test #'string=))))

(ert-deftest emacs-srs-trainer-test-org-deck-selection ()
  (let ((cards (emacs-srs-trainer-due-cards
                t nil nil nil emacs-srs-trainer-org-deck-name)))
    (should (= (length cards)
               (length (emacs-srs-trainer-deck-by-name
                        emacs-srs-trainer-org-deck-name))))
    (should (cl-find "org-export-dispatch" cards
                     :key #'emacs-srs-trainer-card-id
                     :test #'string=))
    (should-not (cl-find "tutorial-move-forward-char" cards
                         :key #'emacs-srs-trainer-card-id
                         :test #'string=))))

(ert-deftest emacs-srs-trainer-test-due-card-limit ()
  (let ((cards (emacs-srs-trainer-due-cards
                t nil nil nil emacs-srs-trainer-info-deck-name 3)))
    (should (= (length cards) 3))))

(ert-deftest emacs-srs-trainer-test-prefix-card-generation ()
  (let ((a (emacs-srs-trainer-deck-generate-prefix-cards))
        (b (emacs-srs-trainer-deck-generate-prefix-cards)))
    (should (equal a b))
    (should (cl-find "C-u 8 C-f" a
                     :key (lambda (card)
                            (plist-get card :canonical-answer))
                     :test #'string=))))

(ert-deftest emacs-srs-trainer-test-main-command-renders-welcome ()
  (let ((emacs-srs-trainer-review-buffer-name
         " *emacs-srs-trainer-main-test*"))
    (unwind-protect
        (let ((buffer (emacs-srs-trainer)))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should (derived-mode-p 'emacs-srs-trainer-welcome-mode))
            (should (string-match-p "Emacs SRS Trainer" (buffer-string)))
            (should (string-match-p "Available decks" (buffer-string)))
            (goto-char (point-min))
            (should (next-button (point-min)))))
      (ignore-errors (kill-buffer emacs-srs-trainer-review-buffer-name)))))

(ert-deftest emacs-srs-trainer-test-review-loop-smoke ()
  (let* ((dir (make-temp-file "emacs-srs-trainer-review-" t))
         (file (expand-file-name "state.el" dir))
         (card (emacs-srs-trainer-test--card "tutorial-move-forward-char"))
         (emacs-srs-trainer-storage-file file)
         (emacs-srs-trainer-read-answer-function (lambda () "C-f"))
         (emacs-srs-trainer-read-continuation-function (lambda () 'quit)))
    (unwind-protect
        (let ((result (emacs-srs-trainer--review-cards
                       (list card)
                       (get-buffer-create " *emacs-srs-trainer-test*"))))
          (should (= (plist-get result :reviewed) 1))
          (should (= (plist-get result :correct) 1))
          (should (plist-get result :quit))
          (should (file-exists-p file))
          (with-current-buffer " *emacs-srs-trainer-test*"
            (should (string-match-p "Emacs SRS Trainer"
                                    (buffer-string)))
            (should (string-match-p "Recently reviewed in this session: 1"
                                    (buffer-string)))
            (should (string-match-p "Available decks"
                                    (buffer-string)))
            (should (string-match-p "Recent reviews"
                                    (buffer-string)))
            (goto-char (point-min))
            (should (next-button (point-min)))))
      (ignore-errors (kill-buffer " *emacs-srs-trainer-test*"))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-info-review-loop-smoke ()
  (let* ((dir (make-temp-file "emacs-srs-trainer-info-review-" t))
         (file (expand-file-name "state.el" dir))
         (card (emacs-srs-trainer-test--card "info-search-text"))
         (emacs-srs-trainer-storage-file file)
         (emacs-srs-trainer-read-answer-function (lambda () "s"))
         (emacs-srs-trainer-read-continuation-function (lambda () 'quit)))
    (unwind-protect
        (let ((result (emacs-srs-trainer--review-cards
                       (list card)
                       (get-buffer-create " *emacs-srs-trainer-info-test*")
                       nil nil
                       emacs-srs-trainer-info-deck-name)))
          (should (= (plist-get result :reviewed) 1))
          (should (= (plist-get result :correct) 1))
          (should (plist-get result :quit))
          (with-current-buffer " *emacs-srs-trainer-info-test*"
            (should (string-match-p "Emacs SRS Trainer"
                                    (buffer-string)))
            (should (string-match-p "Deck: Info: An Introduction"
                                    (buffer-string)))))
      (ignore-errors (kill-buffer " *emacs-srs-trainer-info-test*"))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-review-loop-renders-completion ()
  (let* ((deck-name "Synthetic Complete")
         (card (emacs-srs-trainer-test--synthetic-deck-card
                "synthetic-complete" deck-name "C-f"))
         (dir (make-temp-file "emacs-srs-trainer-complete-" t))
         (file (expand-file-name "state.el" dir))
         (emacs-srs-trainer-decks nil)
         (emacs-srs-trainer-default-deck deck-name)
         (emacs-srs-trainer-shuffle-cards-within-queue nil)
         (emacs-srs-trainer-storage-file file)
         (emacs-srs-trainer-read-answer-function (lambda () "C-f"))
         (emacs-srs-trainer-read-continuation-function (lambda () 'next)))
    (unwind-protect
        (progn
          (emacs-srs-trainer-register-deck deck-name (list card))
          (let ((result (emacs-srs-trainer--review-cards
                         (list card)
                         (get-buffer-create " *emacs-srs-trainer-complete-test*")
                         nil nil deck-name)))
            (should (= (plist-get result :reviewed) 1))
            (should (= (plist-get result :correct) 1))
            (with-current-buffer " *emacs-srs-trainer-complete-test*"
              (should (string-match-p "You have reviewed all due cards"
                                      (buffer-string)))
              (should (string-match-p "m: main menu"
                                      (buffer-string))))))
      (ignore-errors (kill-buffer " *emacs-srs-trainer-complete-test*"))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-completion-action-echo-prompt ()
  (let (prompt)
    (cl-letf (((symbol-function 'read-key)
               (lambda (read-prompt)
                 (setq prompt read-prompt)
                 ?m)))
      (should (eq (emacs-srs-trainer-read-completion-action) 'menu))
      (should (string-match-p "main menu" prompt)))))

(ert-deftest emacs-srs-trainer-test-review-loop-session-limit-completion ()
  (let* ((deck-name "Synthetic Limited")
         (first-card (emacs-srs-trainer-test--synthetic-deck-card
                      "synthetic-limited-1" deck-name "C-f"))
         (second-card (emacs-srs-trainer-test--synthetic-deck-card
                       "synthetic-limited-2" deck-name "C-b"))
         (dir (make-temp-file "emacs-srs-trainer-limited-" t))
         (file (expand-file-name "state.el" dir))
         (emacs-srs-trainer-decks nil)
         (emacs-srs-trainer-default-deck deck-name)
         (emacs-srs-trainer-shuffle-cards-within-queue nil)
         (emacs-srs-trainer-storage-file file)
         (emacs-srs-trainer-read-answer-function (lambda () "C-f"))
         (emacs-srs-trainer-read-continuation-function (lambda () 'next)))
    (unwind-protect
        (progn
          (emacs-srs-trainer-register-deck deck-name
                                           (list first-card second-card))
          (let ((result (emacs-srs-trainer--review-cards
                         (emacs-srs-trainer-due-cards nil nil nil nil
                                                      deck-name 1)
                         (get-buffer-create " *emacs-srs-trainer-limited-test*")
                         nil nil deck-name nil 1)))
            (should (= (plist-get result :reviewed) 1))
            (should (= (plist-get result :correct) 1))
            (with-current-buffer " *emacs-srs-trainer-limited-test*"
              (should (string-match-p "Session limit: 1"
                                      (buffer-string)))
              (should (string-match-p "Session complete. 1 due card remains"
                                      (buffer-string))))))
      (ignore-errors (kill-buffer " *emacs-srs-trainer-limited-test*"))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-review-all-practice-does-not-update-storage ()
  (let* ((deck-name "Synthetic Practice")
         (card (emacs-srs-trainer-test--synthetic-deck-card
                "synthetic-practice" deck-name "C-f"))
         (dir (make-temp-file "emacs-srs-trainer-practice-" t))
         (file (expand-file-name "state.el" dir))
         (initial-state (emacs-srs-trainer-storage-put-card-state
                         "synthetic-practice"
                         (emacs-srs-trainer-scheduler-new-state 1000.0)
                         (emacs-srs-trainer-storage-empty-state)))
         (emacs-srs-trainer-decks nil)
         (emacs-srs-trainer-default-deck deck-name)
         (emacs-srs-trainer-storage-file file)
         (emacs-srs-trainer-read-answer-function (lambda () "C-b"))
         (emacs-srs-trainer-read-continuation-function (lambda () 'quit)))
    (unwind-protect
        (progn
          (emacs-srs-trainer-register-deck deck-name (list card))
          (emacs-srs-trainer-storage-save initial-state file)
          (let ((result (emacs-srs-trainer-review-all 1)))
            (should (= (plist-get result :reviewed) 1))
            (should (plist-get result :practice))
            (should (equal (emacs-srs-trainer-storage-load file)
                           initial-state))
            (with-current-buffer emacs-srs-trainer-review-buffer-name
              (should (string-match-p "Emacs SRS Trainer"
                                      (buffer-string)))
              (should (string-match-p "Recently reviewed in this session: 1"
                                      (buffer-string))))))
      (ignore-errors (kill-buffer emacs-srs-trainer-review-buffer-name))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-review-loop-refreshes-learning-card ()
  (let* ((dir (make-temp-file "emacs-srs-trainer-review-refresh-" t))
         (file (expand-file-name "state.el" dir))
         (card (emacs-srs-trainer-test--card "tutorial-move-forward-char"))
         (now 1000.0)
         (answers '("C-b" "C-f"))
         (continuations 0)
         (emacs-srs-trainer-storage-file file)
         (emacs-srs-trainer-read-answer-function
          (lambda () (pop answers)))
         (emacs-srs-trainer-read-continuation-function
          (lambda ()
            (setq continuations (1+ continuations))
            (if (= continuations 1)
                (progn
                  (setq now (+ now emacs-srs-trainer-scheduler-lapse-delay-seconds 1))
                  'next)
              'quit))))
    (unwind-protect
        (cl-letf (((symbol-function 'emacs-srs-trainer-scheduler-now)
                   (lambda () now)))
          (let ((result
                 (emacs-srs-trainer--review-cards
                  (list card)
                  (get-buffer-create " *emacs-srs-trainer-refresh-test*")
                  (lambda (state refresh-now)
                    (emacs-srs-trainer-due-cards nil nil refresh-now state)))))
            (should (= (plist-get result :reviewed) 2))
            (should (= (plist-get result :correct) 1))
            (should (plist-get result :quit))
            (with-current-buffer " *emacs-srs-trainer-refresh-test*"
              (should (string-match-p "Emacs SRS Trainer"
                                      (buffer-string)))
              (should (string-match-p "Recently reviewed in this session: 2"
                                      (buffer-string))))
            (let ((stored-card-state
                   (emacs-srs-trainer-storage-card-state
                    (emacs-srs-trainer-card-id card)
                    (emacs-srs-trainer-storage-load file)
                    now)))
              (should (= (plist-get stored-card-state :learning-step) 1))
              (should (= (plist-get stored-card-state :due) (+ now 600))))))
      (ignore-errors (kill-buffer " *emacs-srs-trainer-refresh-test*"))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-result-faces ()
  (let* ((card (emacs-srs-trainer-test--card "tutorial-move-forward-char"))
         (buffer (get-buffer-create " *emacs-srs-trainer-face-test*"))
         (correct-grade (emacs-srs-trainer-grade-answer card "C-f"))
         (incorrect-grade (emacs-srs-trainer-grade-answer card "C-b")))
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          (emacs-srs-trainer--render-result buffer card correct-grade)
          (goto-char (point-min))
          (search-forward "Correct.")
          (should (eq (get-text-property (match-beginning 0) 'face)
                      'emacs-srs-trainer-correct-face))
          (erase-buffer)
          (emacs-srs-trainer--render-result buffer card incorrect-grade)
          (goto-char (point-min))
          (search-forward "Incorrect.")
          (should (eq (get-text-property (match-beginning 0) 'face)
                      'emacs-srs-trainer-incorrect-face)))
      (ignore-errors (kill-buffer buffer)))))

(ert-deftest emacs-srs-trainer-test-wrong-answer-shows-learning-delay ()
  (let* ((dir (make-temp-file "emacs-srs-trainer-review-wrong-" t))
         (file (expand-file-name "state.el" dir))
         (card (emacs-srs-trainer-test--card "tutorial-move-forward-char"))
         (now 1000.0)
         (emacs-srs-trainer-storage-file file)
         (emacs-srs-trainer-read-answer-function (lambda () "C-b"))
         (emacs-srs-trainer-read-continuation-function (lambda () 'quit)))
    (unwind-protect
        (cl-letf (((symbol-function 'emacs-srs-trainer-scheduler-now)
                   (lambda () now)))
          (let ((result (emacs-srs-trainer--review-cards
                         (list card)
                         (get-buffer-create " *emacs-srs-trainer-wrong-test*"))))
            (should (= (plist-get result :reviewed) 1))
            (should (= (plist-get result :correct) 0))
            (should (plist-get result :quit))
            (with-current-buffer " *emacs-srs-trainer-wrong-test*"
              (should (string-match-p "Emacs SRS Trainer"
                                      (buffer-string)))
              (should (string-match-p "Redo"
                                      (buffer-string)))
              (should (string-match-p "Next due: in 1 minute"
                                      (buffer-string))))))
      (ignore-errors (kill-buffer " *emacs-srs-trainer-wrong-test*"))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-prefix-answer-capture ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "C-u 8 C-f")) nil)))
    (should (string= (emacs-srs-trainer-read-answer-key-sequence)
                     "C-u 8 C-f"))))

(ert-deftest emacs-srs-trainer-test-continuation-debounce-discards-repeat ()
  (let ((emacs-srs-trainer-continuation-debounce-seconds 0.01)
        (unread-command-events
         (append (listify-key-sequence (kbd "SPC"))
                 (listify-key-sequence (kbd "C-b"))
                 nil)))
    (emacs-srs-trainer--debounce-continuation-event ?\s)
    (should (equal unread-command-events
                   (append (listify-key-sequence (kbd "C-b")) nil)))))

(ert-deftest emacs-srs-trainer-test-continuation-debounce-preserves-other-event ()
  (let ((emacs-srs-trainer-continuation-debounce-seconds 0.01)
        (unread-command-events
         (append (listify-key-sequence (kbd "C-b")) nil)))
    (emacs-srs-trainer--debounce-continuation-event ?\s)
    (should (equal unread-command-events
                   (append (listify-key-sequence (kbd "C-b")) nil)))))

(ert-deftest emacs-srs-trainer-test-review-loop-debounces-space-continuation ()
  (let* ((dir (make-temp-file "emacs-srs-trainer-debounce-" t))
         (file (expand-file-name "state.el" dir))
         (first-card (emacs-srs-trainer-test--card "tutorial-move-forward-char"))
         (second-card (emacs-srs-trainer-test--card "tutorial-move-backward-char"))
         (continuations 0)
         (emacs-srs-trainer-storage-file file)
         (emacs-srs-trainer-continuation-debounce-seconds 0.01)
         (emacs-srs-trainer-read-answer-function
          #'emacs-srs-trainer-read-answer-key-sequence)
         (emacs-srs-trainer-read-continuation-function
          (lambda ()
            (setq continuations (1+ continuations))
            (if (= continuations 1)
                (progn
                  (setq emacs-srs-trainer--last-continuation-event ?\s)
                  (setq unread-command-events
                        (append (listify-key-sequence (kbd "SPC"))
                                (listify-key-sequence (kbd "C-b"))
                                nil))
                  'next)
              'quit)))
         (unread-command-events
          (append (listify-key-sequence (kbd "C-f")) nil)))
    (unwind-protect
        (let ((result (emacs-srs-trainer--review-cards
                       (list first-card second-card)
                       (get-buffer-create " *emacs-srs-trainer-debounce-test*"))))
          (should (= (plist-get result :reviewed) 2))
          (should (= (plist-get result :correct) 2)))
      (ignore-errors (kill-buffer " *emacs-srs-trainer-debounce-test*"))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest emacs-srs-trainer-test-control-g-answer-capture ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "C-g")) nil))
        (quit-flag nil))
    (should (string= (emacs-srs-trainer-read-answer-key-sequence) "C-g"))
    (should-not quit-flag)))

(ert-deftest emacs-srs-trainer-test-compound-answer-capture ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "C-x C-f")) nil))
        (card (emacs-srs-trainer-test--card "tutorial-find-file")))
    (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                     "C-x C-f"))))

(ert-deftest emacs-srs-trainer-test-answer-capture-echoes-progress-and-correct ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "C-x C-f")) nil))
        (card (emacs-srs-trainer-test--card "tutorial-find-file"))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (if (and (string= format-string "%s") args)
                           (car args)
                         (apply #'format-message format-string args))
                       messages))))
      (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                       "C-x C-f")))
    (setq messages (nreverse messages))
    (should (equal (mapcar #'substring-no-properties messages)
                   '("Answer: C-x" "Answer: C-x C-f")))
    (should-not (get-text-property (length "Answer: ") 'face (car messages)))
    (should (eq (get-text-property (length "Answer: ") 'face (cadr messages))
                'emacs-srs-trainer-correct-face))))

(ert-deftest emacs-srs-trainer-test-answer-capture-echoes-wrong-event-only ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "C-x C-s")) nil))
        (card (emacs-srs-trainer-test--card "tutorial-find-file"))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (if (and (string= format-string "%s") args)
                           (car args)
                         (apply #'format-message format-string args))
                       messages))))
      (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                       "C-x C-s")))
    (setq messages (nreverse messages))
    (should (equal (mapcar #'substring-no-properties messages)
                   '("Answer: C-x" "Answer: C-x C-s")))
    (should-not (get-text-property (length "Answer: ") 'face (cadr messages)))
    (should (eq (get-text-property (string-match "C-s" (cadr messages))
                                   'face
                                   (cadr messages))
                'emacs-srs-trainer-incorrect-face))))

(ert-deftest emacs-srs-trainer-test-answer-capture-echo-compacts-typed-text ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "M-x replace-string RET")) nil))
        (card (emacs-srs-trainer-test--card "tutorial-replace-string"))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (if (and (string= format-string "%s") args)
                           (car args)
                         (apply #'format-message format-string args))
                       messages))))
      (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                       "M-x r e p l a c e - s t r i n g RET")))
    (should (equal (substring-no-properties (car messages))
                   "Answer: M-x replace-string RET"))
    (should (eq (get-text-property (length "Answer: ") 'face (car messages))
                'emacs-srs-trainer-correct-face))))

(ert-deftest emacs-srs-trainer-test-answer-capture-prompt-shows-progress ()
  (let ((events (append (listify-key-sequence (kbd "C-x C-f")) nil))
        (card (emacs-srs-trainer-test--card "tutorial-find-file"))
        prompts)
    (cl-letf (((symbol-function 'read-event)
               (lambda (prompt &optional _inherit-input-method _seconds)
                 (push prompt prompts)
                 (pop events)))
              ((symbol-function 'message) #'ignore))
      (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                       "C-x C-f")))
    (should (equal (nreverse prompts)
                   '("Answer: " "Answer: C-x ")))))

(ert-deftest emacs-srs-trainer-test-answer-feedback-clear-is-nonblocking ()
  (let ((noninteractive nil)
        (emacs-srs-trainer-answer-feedback-delay-seconds 0.45)
        (emacs-srs-trainer--answer-feedback-clear-timer nil)
        scheduled)
    (cl-letf (((symbol-function 'sit-for)
               (lambda (&rest _args)
                 (error "answer feedback must not block")))
              ((symbol-function 'run-at-time)
               (lambda (seconds repeat function &rest args)
                 (setq scheduled (list seconds repeat function args))
                 'scheduled-answer-feedback-clear))
              ((symbol-function 'timerp)
               (lambda (timer)
                 (eq timer 'scheduled-answer-feedback-clear)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'message) #'ignore))
      (emacs-srs-trainer--echo-correct-answer (kbd "C-f")))
    (should (equal (car scheduled) 0.45))
    (should (null (cadr scheduled)))
    (should (functionp (nth 2 scheduled)))))

(ert-deftest emacs-srs-trainer-test-answer-capture-redacts-only-wrong-typed-char ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "M-x replace-x")) nil))
        (card (emacs-srs-trainer-test--card "tutorial-replace-string"))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (if (and (string= format-string "%s") args)
                           (car args)
                         (apply #'format-message format-string args))
                       messages))))
      (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                       "M-x r e p l a c e - x")))
    (let* ((final (car messages))
           (plain (substring-no-properties final))
           (replace-start (string-match "replace-" plain))
           (wrong-start (1- (length plain))))
      (should (equal plain "Answer: M-x replace-x"))
      (should-not (get-text-property replace-start 'face final))
      (should (eq (get-text-property wrong-start 'face final)
                  'emacs-srs-trainer-incorrect-face)))))

(ert-deftest emacs-srs-trainer-test-early-wrong-first-key-capture ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "C-x C-f")) nil))
        (card (emacs-srs-trainer-test--synthetic-card "C-g C-k")))
    (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                     "C-x"))
    (should (equal unread-command-events
                   (append (listify-key-sequence (kbd "C-f")) nil)))))

(ert-deftest emacs-srs-trainer-test-early-wrong-after-compound-prefix-capture ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "C-x C-s")) nil))
        (card (emacs-srs-trainer-test--card "tutorial-find-file")))
    (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                     "C-x C-s"))))

(ert-deftest emacs-srs-trainer-test-escape-meta-prefix-capture ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "<ESC> f")) nil))
        (card (emacs-srs-trainer-test--card "tutorial-forward-word")))
    (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                     "M-f"))))

(ert-deftest emacs-srs-trainer-test-control-g-compound-prefix-capture ()
  (let ((unread-command-events
         (append (listify-key-sequence (kbd "C-g C-k")) nil))
        (quit-flag nil)
        (card (emacs-srs-trainer-test--synthetic-card "C-g C-k")))
    (should (string= (emacs-srs-trainer-read-answer-key-sequence card)
                     "C-g C-k"))
    (should-not quit-flag)))

(provide 'emacs-srs-trainer-test)

;;; emacs-srs-trainer-test.el ends here
