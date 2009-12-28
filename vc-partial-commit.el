;;; vc-partialcommit.el -- commit only some changes from a diff

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
  (diff-hunk-next) ;; skip over the header
  (while (not (eobp))
    (diff-apply-hunk)))

(defvar vc-partial-commit-file ()
  "Contains the working file (with only those changes we want to
commit) and its backup (with all the local changes).")

(defun vc-partial-commit ()
  "Commit the changes in the diff (and only those ones)."
  (interactive)
  ;; The current buffer is the diff (as obtained from C-x v = on a
  ;; single file).
  ;; XXX: update to be able to handle several files.
  ;;
  ;; To make a partial commit:
  ;; 1. Rename the file concerned with the diff to filename.mine.XXX
  ;; 2. Revert the file
  ;; 3. Apply the diff to the file, save to disk: the file now contains only
  ;;    the changes we wanted to keep
  ;; 4. Commit the file
  ;; 5. Rename filename.mine.XXX back to filename so we can recover
  ;;    the changes not checked in. This needs to be done in
  ;;    vc-checkin-hook.
  (let* ((diff-buf (current-buffer))
	 (file-buf (save-excursion (diff-goto-source) (current-buffer)))
	 (file (buffer-file-name file-buf))
	 (backup (concat file (make-temp-name ".mine."))))
    (rename-file file backup)
    (message "backup file: %s" backup)
    (vc-revert-file file)
    (diff-apply-patch)
    (switch-to-buffer file-buf)
    (save-buffer)
    (vc-checkin (list file) (vc-backend file))
    (setq vc-partial-commit-file (list file backup))
    (add-hook 'vc-checkin-hook 'vc-partial-commit-restore-working-file)
    (switch-to-buffer "*VC-log*")
    (display-buffer diff-buf)))

(defun vc-partial-commit-restore-working-file ()
  "Used as a hook to restore the working file (the one that
contains all of our changes) over the temp file containing only
the changes we wanted to commit."
  (let ((file (nth 0 vc-partial-commit-file))
	(backup (nth 1 vc-partial-commit-file)))
    (rename-file backup file t)
    (switch-to-buffer (find-buffer-visiting file))
    (revert-buffer nil t)
    (setq vc-partial-commit-file nil)
    (remove-hook 'vc-checkin-hook 'vc-partial-commit-restore-working-file)))
