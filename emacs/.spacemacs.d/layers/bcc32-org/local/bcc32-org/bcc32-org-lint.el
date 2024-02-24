;;; bcc32-org-lint.el --- bcc32's org-mode lints  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aaron L. Zeng

;; Author: Aaron L. Zeng <me@bcc32.com>
;; Keywords: convenience, languages

;;; Commentary:

;;

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-lint)

(defun bcc32-org-lint--entry-has-todo-children-p (elt)
  "Return non-nil if the entry ELT has a child with any TODO state."
  (seq-some
   (lambda (child)
     (org-element-property :todo-keyword child))
   (org-element-contents elt)))

(defun bcc32-org-lint--lint-headline-statistics-cookie (elt)
  "Return non-nil if the heading ELT is missing a statistics cookie.

The return value is a list (POS MESSAGE) containing the start
position of ELT and an error message, suitable for
`org-lint-add-checker'.

Only returns non-nil if this heading is missing a statistics
cookie but should have one, i.e., if it has children with TODO
keywords."
  (when (and (bcc32-org-lint--entry-has-todo-children-p elt)
             (not (assoc 'statistics-cookie (org-element-property :title elt))))
    (list (org-element-property :begin elt) "heading has no statistics cookie")))

(defun bcc32-org-lint-statistics-cookies (parse-tree)
  "Report missing statistics cookies for headings with TODO subitems.

PARSE-TREE should be an Org-mode parse tree."
  (org-element-map parse-tree '(headline)
    #'bcc32-org-lint--lint-headline-statistics-cookie))

(org-lint-add-checker 'bcc32-org-statistics-cookies
  "Report missing statistics cookies for headings with TODO subitems"
  #'bcc32-org-lint-statistics-cookies
  :categories '(bcc32-org))

(provide 'bcc32-org-lint)
;;; bcc32-org-lint.el ends here
