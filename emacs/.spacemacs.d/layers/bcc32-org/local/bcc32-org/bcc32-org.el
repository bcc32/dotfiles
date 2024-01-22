;;; bcc32-org.el --- Personal linter for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron L. Zeng <me@bcc32.com>

;; Author: Aaron L. Zeng <me@bcc32.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.16.0") (emacs "27.1") (f "0.20.0") (flycheck "31") (magit "2.11.0") (org "9.4.0") (projectile "2.2.0") (s "1.12.0"))
;; URL: https://github.com/bcc32/dotfiles

;;; Commentary:

;; A personal package for various org-mode customizations.

;;; Code:

(require 'dash)
(require 'f)
(require 'magit-core)
(require 'org)
(require 'org-agenda)
(require 'projectile)
(require 's)

(require 'bcc32-org-flycheck)

(defgroup bcc32-org nil "Bcc32's `org-mode' customizations."
  :group 'emacs)

(defvar org-indent-mode)

(define-advice org-align-tags (:around (f &rest args) bcc32-org--disable-org-indent-mode)
  "Ignore the value of `org-indent-mode' while aligning tags."
  (let (org-indent-mode)
    (apply f args)))

(defun bcc32-org--archive-file-p (file-name)
  "Return t if FILE-NAME refers to an *.org_archive file."
  (f-ext? file-name "org_archive"))

(defalias 'bcc32-org--fold-show-all
  (if (functionp 'org-fold-show-all)
      #'org-fold-show-all
    #'org-show-all))

;;;###autoload
(defun bcc32-org-cleanup ()
  "Run some cleanup on the current buffer, if it's an org buffer.

- Update dynamic blocks (e.g., clock reports)
- Update statistics cookies (e.g., [2/3])
- Align heading tags"
  (interactive "*")
  (when (and (derived-mode-p 'org-mode)
             (not (-when-let (file-name (buffer-file-name))
                    (bcc32-org--archive-file-p file-name))))
    (org-map-dblocks
     (lambda ()
       (unless (member "ARCHIVE" (org-get-tags))
         (org-update-dblock))))
    (org-update-statistics-cookies t)
    ;; FIXME: This workaround for incorrect alignment in org headings containing
    ;; links doesn't work.  The key point is that `current-column' returns a
    ;; different result when the point is visible (and the link is rendered like
    ;; so) vs. when the point is not visible (and the link is rendered as raw
    ;; org markup).
    ;;
    ;; I also noticed that `current-column' has spooky side effects: if you
    ;; place point at the end of a heading with a link, then call "M-:
    ;; (current-column)", you get some small-ish number.  Press S-tab to hide
    ;; all entries, then evaluate `current-column' again.  You get a different,
    ;; larger number.  Press S-tab twice more to reveal point.  Point has moved.
    (org-save-outline-visibility :use-markers
      (bcc32-org--fold-show-all)
      (org-align-tags :all))))

(defun bcc32-org--sort-by-closed-getkey ()
  "Return the CLOSED property of the org entry at point.

For CLOSED entries, time is returned as a floating-point time,
else +INF for entries with a todo keyword, -INF otherwise."
  (-if-let (closed (org-entry-get nil "CLOSED"))
      (org-2ft closed)
    (if (org-get-todo-state)
        1.0e+INF
      -1.0e+INF)))

;;;###autoload
(defun bcc32-org-sort-by-closed ()
  "Sort org entry at point by the CLOSED property."
  (interactive "*")
  (org-sort-entries nil ?f 'bcc32-org--sort-by-closed-getkey))

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

;;;###autoload
(defun bcc32-org-lint-agenda-buffers ()
  "Run `org-lint' in all org agenda files, stopping at the first error."
  (interactive)
  (dolist (buf (org-buffer-list 'agenda))
    (set-buffer buf)
    (call-interactively 'org-lint)
    (when (> (buffer-size) 0)
      (error "Lint found errors in buffer"))))

(defun bcc32-org--auto-ingest-init-org-hook ()
  "Ingest an org file if it is named init.org."
  (-when-let (file-name (buffer-file-name))
    (when (string= (f-filename file-name) "init.org")
      (org-babel-lob-ingest file-name))))

(autoload 'magit-merge-in-progress-p "magit-merge")
(autoload 'magit-rebase-in-progress-p "magit-sequence")

;;;###autoload
(defun bcc32-org-commit-and-push-all ()
  "Commit all changed files, pull --rebase, and push the current repo.

If pulling moved the HEAD to a different revision, prompt to revert all buffers
in the current repo."
  (interactive)
  (when (or (magit-merge-in-progress-p)
            (magit-rebase-in-progress-p))
    (user-error "Merge or rebase in progress; aborting"))
  (message "Committing and pushing...")
  (magit-maybe-save-repository-buffers)
  (when (magit-anything-modified-p)
    (magit-git "commit" "-am" "_"))
  (let (head-before head-after)
    (setq head-before (magit-rev-parse (magit-headish)))
    (magit-git "pull" "--rebase")
    (magit-git "push")
    (setq head-after (magit-rev-parse (magit-headish)))
    (when (and (not (magit-rev-eq head-before head-after))
               (y-or-n-p "Revert all project buffers? "))
      (projectile-process-current-project-buffers-current
       (lambda ()
         (when (funcall (or buffer-stale-function #'buffer-stale--default-function) nil)
           (revert-buffer :ignore-auto :no-confirm))))))
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
  "Execute babel source blocks in the subtree of the heading in this agenda buffer."
  (interactive)
  (let* ((blocks-executed 0)
         (org-babel-after-execute-hook
          (cons (lambda () (cl-incf blocks-executed))
                org-babel-after-execute-hook)))
    (org-agenda-with-point-at-orig-entry nil
      (org-babel-execute-subtree))
    (if (cl-plusp blocks-executed)
        (org-agenda-todo 'done)
      (user-error "No blocks executed, so not marking DONE"))))

(provide 'bcc32-org)

;;; bcc32-org.el ends here
