;;; bcc32-org.el --- Personal linter for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron L. Zeng <me@bcc32.com>

;; Author: Aaron L. Zeng <me@bcc32.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "2.11.0") (org "9.6.0") (uuidgen "1.0"))
;; URL: https://github.com/bcc32/dotfiles

;;; Commentary:

;; A personal package for various org-mode customizations.

;;; Code:

(require 'magit-core)
(require 'org)
(require 'org-agenda)
(require 'uuidgen)

(require 'bcc32-org-lint)

(defgroup bcc32-org nil "Bcc32's `org-mode' customizations."
  :group 'emacs)

(defun bcc32-org--archive-file-p (file-name)
  "Return non-nil if FILE-NAME refers to an *.org_archive file."
  (string-match-p (rx ".org_archive" eos) file-name))

;;;###autoload
(defun bcc32-org-cleanup ()
  "Run some cleanup on the current buffer, if it's an org buffer.

- Update dynamic blocks (e.g., clock reports)
- Update statistics cookies (e.g., [2/3])"
  (interactive "*")
  (when (and (derived-mode-p 'org-mode)
             (not (when-let ((file-name (buffer-file-name)))
                    (bcc32-org--archive-file-p file-name))))
    (org-map-dblocks
     (lambda ()
       (unless (member "ARCHIVE" (org-get-tags))
         (org-update-dblock))))
    (org-update-statistics-cookies t)))

(defun bcc32-org-cleanup-entire-agenda ()
  "Clean up all org agenda buffers.

See `bcc32-org-cleanup'."
  (interactive)
  (dolist-with-progress-reporter (b (org-buffer-list 'agenda))
      "Cleaning up agenda files..."
    (with-current-buffer b
      (bcc32-org-cleanup))))

(defun bcc32-org--sort-by-closed-getkey ()
  "Return the CLOSED property of the org entry at point.

For CLOSED entries, time is returned as a floating-point time,
else +INF for entries with a todo keyword, -INF otherwise."
  (if-let ((closed (org-entry-get nil "CLOSED")))
      (org-2ft closed)
    (if (org-get-todo-state)
        1.0e+INF
      -1.0e+INF)))

;;;###autoload
(defun bcc32-org-sort-by-closed ()
  "Sort org entry at point by the CLOSED property."
  (interactive "*")
  (org-sort-entries nil ?f #'bcc32-org--sort-by-closed-getkey))

;;;###autoload
(defun bcc32-org-sort-entire-agenda ()
  "Sort all agenda subtrees by CLOSED."
  (interactive)
  (cl-flet ((sort-if-nonempty
              ()
              (condition-case e
                  (bcc32-org-sort-by-closed)
                (user-error
                 (unless (string= (cadr e) "Nothing to sort")
                   (signal (car e) (cadr e)))))))
    (save-excursion
      (dolist (buf (org-buffer-list 'agenda))
        (set-buffer buf)
        (goto-char (point-min))
        (sort-if-nonempty)
        (org-map-entries #'sort-if-nonempty t nil)))))

(defun bcc32-org--auto-ingest-init-org-hook ()
  "Ingest an org file if it is named init.org."
  (when-let ((file-name (buffer-file-name))
             ((string= (file-name-nondirectory file-name) "init.org")))
    (org-babel-lob-ingest file-name)))

(autoload 'magit-merge-in-progress-p "magit-merge")
(autoload 'magit-rebase-in-progress-p "magit-sequence")

;;;###autoload
(defun bcc32-org-commit-and-push-all ()
  "Commit all changed files, pull --rebase, and push the current repo."
  (interactive)
  (when (or (magit-merge-in-progress-p)
            (magit-rebase-in-progress-p))
    (user-error "Merge or rebase in progress; aborting"))
  (message "Committing and pushing...")
  (magit-maybe-save-repository-buffers)
  (when (magit-anything-modified-p)
    (magit-git "commit" "-am" "_"))
  (magit-git "pull" "--rebase" "-Xignore-space-change")
  (magit-git "push")
  (message "Committing and pushing... done"))

(defcustom bcc32-org-always-skip-weekends nil
  "If non-nil, always reschedule events to the next weekday."
  :type 'boolean
  :group 'bcc32-org)

;;;###autoload
(defun bcc32-org-todo-repeat-maybe-skip-weekends ()
  "Reschedule the current org entry to the next weekday.

Only affects entries with the SKIP_WEEKENDS property set to
non-nil.  Suggested usage is to add this function to
`org-todo-repeat-hook'."
  (when (or bcc32-org-always-skip-weekends (org-entry-get nil "SKIP_WEEKENDS" :inherit))
    (org-back-to-heading)
    (re-search-forward org-scheduled-time-regexp (org-entry-end-position))
    (goto-char (match-beginning 1))
    (while (member (nth 6 (parse-time-string (org-entry-get nil "SCHEDULED")))
                   '(0 6))
      (org-timestamp-change 1 'day))))

;;;###autoload
(defun bcc32-org-agenda-babel-execute-subtree-and-done ()
  "Execute babel source blocks in the subtree of the heading in this agenda buffer.

Signal an error if there are no source blocks in the subtree.

Otherwise, mark the heading DONE after executing source blocks,
if there were no errors during execution."
  (interactive)
  (let* ((executed-any-blocks-p nil)
         (org-babel-after-execute-hook
          (cons (lambda () (setq executed-any-blocks-p t))
                org-babel-after-execute-hook)))
    (org-agenda-with-point-at-orig-entry nil
      (org-babel-execute-subtree))
    (if executed-any-blocks-p
        (org-agenda-todo 'done)
      (user-error "No blocks executed, so not marking DONE"))))

;;;###autoload
(defun bcc32-org-update-blocked-tasks ()
  "Execute babel source blocks in entries with todo keyword BLOCKED.

If the results change for a given entry, add the FLAGGED tag to the
entry.

If any FLAGGED tags were added, list the entries."
  (interactive)
  (let ((org-confirm-babel-evaluate nil)
        (entries-flagged 0))
    (message "Updating blocked tasks...")
    (save-window-excursion
      (org-map-entries
       (lambda ()
         ;; Make sure buffer containing code block is visible; otherwise the
         ;; query (if a code block has `:eval query') is asking about an
         ;; invisible code block.
         (let ((buffer (current-buffer)))
           (pop-to-buffer-same-window buffer)
           (let* ((before (save-restriction
                            (org-narrow-to-subtree)
                            (buffer-string)))
                  (_ (org-babel-execute-subtree))
                  (after (save-restriction
                           (org-narrow-to-subtree)
                           (buffer-string))))
             (unless (string= (string-trim before) (string-trim after))
               (cl-incf entries-flagged)
               (org-toggle-tag "FLAGGED" 'on)))))
       "/BLOCKED"
       'agenda
       'archive))
    (message "Updating blocked tasks... done.  %d entries updated." entries-flagged)
    (when (cl-plusp entries-flagged)
      (run-with-idle-timer 0 nil #'org-tags-view nil "+FLAGGED"))))

;;;###autoload
(defun bcc32-org-ensure-custom-id ()
  "If the org entry at point does not have a CUSTOM_ID property, add one."
  (interactive)
  (if (org-entry-get nil "CUSTOM_ID")
      (message "Entry already has CUSTOM_ID")
    (let ((uuid (uuidgen-4)))
      (org-set-property "CUSTOM_ID" uuid)
      (message "CUSTOM_ID set to %s" uuid))))

;;;###autoload
(defun bcc32-org-set-default-directory-to-todo ()
  "Set `default-directory' to ~/todo.

Used as a hook to ensure that my Org Agenda buffers have a predictable
`default-directory' (so that I can comfortably run commands in them)."
  (setq default-directory "~/todo/"))

(provide 'bcc32-org)

;;; bcc32-org.el ends here
