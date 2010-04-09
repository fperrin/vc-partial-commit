;;; vc-partial-commit.el -- commit only some changes from a diff

;; The idea is that after some happy hacking on a file, there are
;; several unrelated changes in our working copy. For instance, while
;; working on feature A, we hit bug B, so we corrected it before
;; continuing to work on A. But, we want to commit "atomic changes"
;; (e.g. only correction to B), so we want to commit only part of
;; thoses changes (as in "git add --patch"). So, we ask for the diff
;; between our working copy and the last updated file, we keep only
;; those hunks we want to keep (in diff-mode: k to delete the hunks we
;; don't want to commit now, for instance), and commit that. And then
;; we can make a second commit with the rest of the changes.

(setq debug-on-error t)

(defun diff-apply-patch ()
  "Apply all the hunks of the diff found in the current buffer."
  (goto-char (point-min))
  (diff-hunk-next) ;; skip over the header of the first file
  (while (not (eobp))
    (diff-apply-hunk)))

(defvar vc-partial-commit-files ()
  "Alist of working files to be commited with the working file as
the key and the backup file (with all the modifications) as the
value.")

(defun vc-partial-commit ()
  "Commit the changes in the diff (and only those ones). Must be
called from the *vc-diff* buffer."
  (interactive)
  ;; The current buffer is the diff (as obtained from C-x v = on a
  ;; single file or on a selection of files in *vc-dir*).
  ;;
  ;; To make a partial commit, when "git add --patch" or "bzr shelf"
  ;; is not available:
  ;; 1. Copy the file concerned with the diff to filename.mine.XXX
  ;; 2. Revert the file
  ;; 3. Apply the diff to the file, save to disk: the file now contains only
  ;;    the changes we wanted to keep
  ;; 4. Commit the file
  ;; 5. Rename filename.mine.XXX back to filename so we can recover
  ;;    the changes not checked in. This needs to be done in
  ;;    vc-checkin-hook.
  (if vc-partial-commit-files
      (error "vc-partial-commit-files not empty: current commit in progress?"))
  (if (not (eq major-mode 'diff-mode))
      (error "vc-partial-commit-files should be called from the buffer containing the diff to commit"))
  (let* ((diff-buf (current-buffer)) files file backup)
    ;; Get the list of files to commit
    (goto-char (point-min))
    (while (re-search-forward diff-file-header-re nil t)
      (save-excursion
	(diff-goto-source)
	(setq file (buffer-file-name (current-buffer)))
	(setq vc-partial-commit-files
	      (cons (cons file
			  (concat file (make-temp-name ".mine.")))
		    vc-partial-commit-files))))
    (setq files (mapcar 'car vc-partial-commit-files))

    ;; 1. and 2.: rename the files (with all the modifications) to a
    ;; temporary backup, then revert the file.
    (dolist (elem vc-partial-commit-files)
      (setq file (car elem)
	    backup (cdr elem))
      (copy-file file backup)
      (vc-revert-file file))
    ;; 3. apply only those changes we kept in the VC-diff buffer
    (diff-apply-patch)
    (save-some-buffers t (lambda () (member (buffer-file-name) files)))
    ;; 4. commit the changes we kept
    (vc-checkin files (vc-backend (car files)))
    ;; 5. hook a function to restore all the modifications to our
    ;;    working files from the backup, and allow the user to enter
    ;;    its changelog
    (add-hook 'vc-checkin-hook 'vc-partial-commit-restore-working-file)
    (switch-to-buffer "*VC-log*")
    (display-buffer diff-buf)))

(defun vc-partial-commit-or-diff ()
  "If we are looking at a diff, then commit this diff; otherwise, ask for 
a diff.

XXX: this should really be integrated with vc-next-action."
  (interactive)
  (if (eq major-mode 'diff-mode)
      (vc-partial-commit)
    (vc-diff nil)))

(define-key vc-prefix-map "p" 'vc-partial-commit-or-diff)

(defun vc-partial-commit-restore-working-file ()
  "Used as a hook to restore the working file (the one that
contains all of our changes) over the temp file (containing only
the changes we just committed)."
  ;; 5. Rename filename.mine.XXX back to filename so we can recover
  ;;    the changes not checked in.
  (dolist (elem vc-partial-commit-files)
    (let ((file (car elem)) (backup (cdr elem)))
      (rename-file backup file t)
      (switch-to-buffer (find-buffer-visiting file))
      (revert-buffer nil t)))
  (setq vc-partial-commit-files nil)
  (remove-hook 'vc-checkin-hook 'vc-partial-commit-restore-working-file))

(provide 'vc-partial-commit)

;;; vc-partial-commit.el ends here
