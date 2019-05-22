;;; bcc32-org.el --- Personal linter for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron L. Zeng <me@bcc32.com>

;; Author: Aaron L. Zeng <me@bcc32.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (flycheck "31") (org "9.1.0"))
;; URL: https://github.com/bcc32/dotfiles

;;; Commentary:

;; TODO commentary

;;; Code:

(require 'flycheck)
(require 'org)
(require 'seq)

;;;###autoload
(defun bcc32-org-cleanup ()
  "Run some cleanup on the current buffer, if it's an org buffer.

- Update dynamic blocks (e.g., clock reports)
- Update statistics cookies (e.g., [2/3])
- Align heading tags"
  (interactive "*")
  (when (derived-mode-p 'org-mode)
    (org-update-all-dblocks)
    (org-update-statistics-cookies t)
    (org-align-tags :all)))

(defun bcc32-org--map-entry-children (proc)
  "Call PROC with point at each child of the current entry.

Collect the values returned by PROC into a list."
  (save-excursion
    (let (results)
      (when (org-goto-first-child)
        (push (funcall proc) results)
        (while (org-goto-sibling)
          (push (funcall proc) results)))
      (nreverse results))))

(defun bcc32-org--entry-has-todo-children-p ()
  "Return non-nil if the current entry has a child with any TODO state."
  (seq-some #'identity (bcc32-org--map-entry-children #'org-get-todo-state)))

(defconst bcc32-org--statistics-cookie-re
  (rx "[" (0+ digit) (or "%" (: "/" (0+ digit))) "]")
  "Pattern that matches `org-mode' statistics cookies.

Copied from `org-element-statistics-cookie-parser'.")

(defun bcc32-org--flycheck-lint-headline-statistics-cookie (checker)
  "Lint the current headline for statistics cookies using CHECKER."
  (let ((heading (org-get-heading :no-tags :no-todo)))
    (when (and (bcc32-org--entry-has-todo-children-p)
               (not (string-match-p bcc32-org--statistics-cookie-re heading)))
      (flycheck-error-new-at (line-number-at-pos)
                             nil
                             'error
                             "heading has no statistics cookie"
                             :checker checker))))

(defun bcc32-org--flycheck-lint-buffer (checker)
  "Lint the current buffer for statistics cookies using CHECKER."
  (delq nil
        (org-map-entries
         (lambda ()
           (bcc32-org--flycheck-lint-headline-statistics-cookie checker)))))

(defun bcc32-org--org-mode-use-flycheck-as-next-error-function ()
  "Use `flycheck-next-error' as the `next-error-function'.

This is so that `next-error-function' in `org-mode' buffers is
set correctly."
  (setq next-error-function 'flycheck-next-error))

;;;###autoload
(defun bcc32-org-flycheck-start (checker callback)
  "Start linting an org buffer for syntax checker CHECKER.

CALLBACK should be a flycheck checker callback."
  (condition-case err
      (let ((errors (bcc32-org--flycheck-lint-buffer checker)))
        (funcall callback 'finished errors))
    (error (funcall callback 'errored (error-message-string err)))))

;; CR azeng: This should be defined in some autoload or use-package somewhere?
(flycheck-define-generic-checker 'org-lint
  "An org linter to enforce statistics cookies where appropriate."
  :start #'bcc32-org-flycheck-start
  :modes 'org-mode
  :predicate #'org-agenda-file-p)
(add-to-list 'flycheck-checkers 'org-lint)
;; CR azeng: This doesn't actually work because org loads the agenda files
;; before this file gets required.
;;
;; It should be moved to a spacemacs layer post-init function or something.
(add-hook 'org-mode-hook 'bcc32-org--org-mode-use-flycheck-as-next-error-function)

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

(provide 'bcc32-org)

;;; bcc32-org.el ends here
