;;; bcc32-org-lint.el --- bcc32's org-mode lints  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aaron L. Zeng

;; Author: Aaron L. Zeng <me@bcc32.com>
;; Keywords: convenience, languages

;;; Commentary:

;;

;;; Code:

(require 'org)
(require 'org-lint)

(defun bcc32-org-lint--map-entry-children (proc)
  "Call PROC with point at each child of the current entry.

Collect the values returned by PROC into a list."
  (save-excursion
    (let (results)
      (when (org-goto-first-child)
        (push (funcall proc) results)
        (while (org-goto-sibling)
          (push (funcall proc) results)))
      (nreverse results))))

(defun bcc32-org-lint--entry-has-todo-children-p ()
  "Return non-nil if the current entry has a child with any TODO state."
  (cl-some #'identity (bcc32-org-lint--map-entry-children #'org-get-todo-state)))

(defconst bcc32-org-lint--statistics-cookie-regexp
  (rx "[" (0+ digit) (or "%" (: "/" (0+ digit))) "]")
  "Pattern that matches `org-mode' statistics cookies.

Copied from `org-element-statistics-cookie-parser'.")

(defun bcc32-org-lint--lint-headline-statistics-cookie ()
  "Return non-nil if the heading at point is missing a statistics cookie.

The return value is a list (POS MESSAGE) containing the current
position and an error message, suitable for
`org-lint-add-checker'.

Only returns non-nil if this heading is missing a statistics
cookie but should have one, i.e., if it has children with TODO
keywords."
  (let ((heading (org-get-heading :no-tags :no-todo)))
    (when (and (bcc32-org-lint--entry-has-todo-children-p)
               (not (string-match-p bcc32-org-lint--statistics-cookie-regexp heading)))
      (list (point) "heading has no statistics cookie"))))

;; TODO: Use parse-tree instead of crawling the buffer by hand
(defun bcc32-org-lint-statistics-cookies (_parse-tree)
  "Report missing statistics cookies for headings with TODO subitems."
  (delq nil
        (org-map-entries
         (lambda ()
           (bcc32-org-lint--lint-headline-statistics-cookie)))))

(org-lint-add-checker 'bcc32-org-statistics-cookies
  "Report missing statistics cookies for headings with TODO subitems"
  #'bcc32-org-lint-statistics-cookies
  :categories '(bcc32-org))

(provide 'bcc32-org-lint)
;;; bcc32-org-lint.el ends here
