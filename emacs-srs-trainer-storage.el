;;; emacs-srs-trainer-storage.el --- Local progress persistence  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Plain Lisp-file storage for review progress.  The shape is small and
;; intentionally separate from the scheduler so future SQLite or Anki
;; import/export integrations can reuse the same state boundaries.

;;; Code:

(require 'cl-lib)
(require 'emacs-srs-trainer-scheduler)

(defgroup emacs-srs-trainer-storage nil
  "Storage settings for `emacs-srs-trainer'."
  :group 'emacs-srs-trainer)

(defcustom emacs-srs-trainer-storage-file
  (expand-file-name "emacs-srs-trainer-state.el" user-emacs-directory)
  "Plain Lisp file used to store spaced-repetition progress."
  :type 'file)

(defconst emacs-srs-trainer-storage-version 1
  "Storage schema version.")

(defun emacs-srs-trainer-storage-empty-state ()
  "Return an empty storage state."
  (list :version emacs-srs-trainer-storage-version
        :cards nil))

(defun emacs-srs-trainer-storage-load (&optional file)
  "Load storage state from FILE.

If FILE does not exist, return an empty state."
  (let ((path (or file emacs-srs-trainer-storage-file)))
    (if (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (let ((state (read (current-buffer))))
            (unless (and (listp state) (plist-member state :cards))
              (error "Invalid emacs-srs-trainer storage file: %s" path))
            state))
      (emacs-srs-trainer-storage-empty-state))))

(defun emacs-srs-trainer-storage-save (state &optional file)
  "Persist STATE to FILE atomically."
  (let* ((path (or file emacs-srs-trainer-storage-file))
         (dir (file-name-directory path)))
    (make-directory dir t)
    (let ((temp (make-temp-file (expand-file-name ".emacs-srs-trainer-state-" dir))))
      (unwind-protect
          (progn
            (with-temp-file temp
              (let ((print-length nil)
                    (print-level nil))
                (prin1 state (current-buffer))
                (insert "\n")))
            (rename-file temp path t))
        (when (file-exists-p temp)
          (ignore-errors (delete-file temp))))))
  state)

(defun emacs-srs-trainer-storage-card-state (card-id &optional state now)
  "Return scheduler state for CARD-ID from STATE.

If no state exists, return a new due scheduler state at NOW."
  (let* ((loaded (or state (emacs-srs-trainer-storage-load)))
         (cards (plist-get loaded :cards))
         (entry (assoc-string card-id cards t)))
    (if entry
        (cdr entry)
      (emacs-srs-trainer-scheduler-new-state now))))

(defun emacs-srs-trainer-storage-put-card-state (card-id card-state &optional state)
  "Return STATE updated with CARD-ID set to CARD-STATE."
  (let* ((loaded (or state (emacs-srs-trainer-storage-load)))
         (cards (copy-sequence (plist-get loaded :cards)))
         (entry (assoc-string card-id cards t)))
    (if entry
        (setcdr entry card-state)
      (push (cons card-id card-state) cards))
    (plist-put loaded :cards cards)))

(defun emacs-srs-trainer-storage-review-card (card-id correct &optional state now)
  "Update CARD-ID in STATE for review result CORRECT.

Return the updated storage state."
  (let* ((loaded (or state (emacs-srs-trainer-storage-load)))
         (old-card-state (emacs-srs-trainer-storage-card-state card-id loaded))
         (new-card-state (emacs-srs-trainer-scheduler-review old-card-state correct now)))
    (emacs-srs-trainer-storage-put-card-state card-id new-card-state loaded)))

(defun emacs-srs-trainer-storage-reset (&optional file)
  "Delete storage FILE and return an empty state."
  (let ((path (or file emacs-srs-trainer-storage-file)))
    (when (file-exists-p path)
      (delete-file path))
    (emacs-srs-trainer-storage-empty-state)))

(defun emacs-srs-trainer-storage-health (&optional file)
  "Return a plist describing storage health for FILE."
  (let ((path (or file emacs-srs-trainer-storage-file)))
    (condition-case err
        (let ((state (emacs-srs-trainer-storage-load path)))
          (list :ok t
                :path path
                :card-state-count (length (plist-get state :cards))
                :message "ok"))
      (error
       (list :ok nil
             :path path
             :card-state-count 0
             :message (error-message-string err))))))

(provide 'emacs-srs-trainer-storage)

;;; emacs-srs-trainer-storage.el ends here
