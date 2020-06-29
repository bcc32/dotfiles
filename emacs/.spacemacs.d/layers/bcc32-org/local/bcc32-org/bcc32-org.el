;;; bcc32-org.el --- Personal linter for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron L. Zeng <me@bcc32.com>

;; Author: Aaron L. Zeng <me@bcc32.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.16.0") (emacs "24.1") (f "0.20.0") (flycheck "31") (magit "2.11.0") (org "9.1.0") (s "1.12.0"))
;; URL: https://github.com/bcc32/dotfiles

;;; Commentary:

;; TODO commentary

;;; Code:

(require 'dash)
(require 'f)
(require 'flycheck)
(require 'magit)
(require 'org)
(require 's)

(defun bcc32-org--archive-file-p (file-name)
  "Return t if FILE-NAME refers to an *.org_archive file."
  (f-ext? file-name "org_archive"))

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
    (org-update-all-dblocks)
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
      (org-show-all)
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
  (save-excursion
    (dolist (buf (org-buffer-list 'agenda))
      (set-buffer buf)
      (goto-char (point-min))
      (ignore-errors (bcc32-org-sort-by-closed))
      (org-map-entries (lambda () (ignore-errors (bcc32-org-sort-by-closed)))
                       t
                       nil))))

;;;###autoload
(defun bcc32-org-lint-all-open-buffers ()
  "Run `org-lint' on all org buffers, stopping at the first error."
  (interactive)
  (catch 'found-error
    (dolist (buf (org-buffer-list))
      (set-buffer buf)
      (call-interactively 'org-lint)
      (when (> (buffer-size) 0)
        (throw 'found-error t)))))

(defun bcc32-org--auto-ingest-init-org-hook ()
  "Ingest an org file if it is named init.org."
  (-when-let (file-name (buffer-file-name))
    (when (string= (f-filename file-name) "init.org")
      (org-babel-lob-ingest file-name))))

(defun bcc32-org--magit-call-git (&rest args)
  "Similar to `magit-call-git', but signal an error when git exits non-zero."
  (unless (zerop (apply #'magit-call-git args))
    (magit-process-buffer)
    (error "git exited non-zero: git %s" (s-join " " args))))

;;;###autoload
(defun bcc32-org-commit-and-push-all ()
  "Commit all changes, pull --rebase, and push the current repo."
  (interactive)
  (message "Committing and pushing...")
  (when (magit-git-string-p "status" "--porcelain")
    (bcc32-org--magit-call-git "commit" "-am" "_"))
  (bcc32-org--magit-call-git "pull" "--rebase")
  (bcc32-org--magit-call-git "push")
  (message "Committing and pushing... done"))

(provide 'bcc32-org)

;;; bcc32-org.el ends here
