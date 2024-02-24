;;; bcc32-org-lint.el --- bcc32's org-mode lints  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aaron L. Zeng

;; Author: Aaron L. Zeng <me@bcc32.com>
;; Keywords: convenience, languages

;;; Commentary:

;; Loading this file registers bcc32's custom linters with org-lint.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-lint)

(defun bcc32-org-lint--entry-has-todo-children-p (elt)
  "Return non-nil if the entry ELT has a child with any TODO state."
  (seq-some
   (apply-partially #'org-element-property :todo-keyword)
   (org-element-contents elt)))

(defun bcc32-org-lint-statistics-cookies (parse-tree)
  "Report missing statistics cookies for headings with TODO subitems.

PARSE-TREE should be an Org-mode parse tree."
  (org-element-map parse-tree 'headline
    (lambda (elt)
      (and (bcc32-org-lint--entry-has-todo-children-p elt)
           (not (org-element-map (org-element-property :title elt)
                    'statistics-cookie
                  #'identity))
           (list (org-element-property :begin elt)
                 "heading has no statistics cookie")))))

(org-lint-add-checker 'bcc32-org-lint-statistics-cookies
  "Report missing statistics cookies for headings with TODO subitems"
  #'bcc32-org-lint-statistics-cookies
  :categories '(bcc32-org))

(provide 'bcc32-org-lint)
;;; bcc32-org-lint.el ends here
