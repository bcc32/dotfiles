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

(defconst bcc32-org-lint-skip-statistics-cookies-property
  "BCC32_SKIP_STATISTICS_COOKIES"
  "If a headline has this property, do not check statistics cookies.")

(defconst bcc32-org-lint--skip-statistics-cookies-property-key
  (intern (concat ":" bcc32-org-lint-skip-statistics-cookies-property)))

(defun bcc32-org-lint--skip-check-statistics-cookies-p (elt)
  "Return nil if the entry ELT should be checked for statistics cookies."
  (or (org-element-property bcc32-org-lint--skip-statistics-cookies-property-key elt)
      (when-let* ((parent (org-element-property :parent elt)))
        (bcc32-org-lint--skip-check-statistics-cookies-p parent))))

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
      (and (not (bcc32-org-lint--skip-check-statistics-cookies-p elt))
           (bcc32-org-lint--entry-has-todo-children-p elt)
           (not (org-element-map (org-element-property :title elt)
                    'statistics-cookie
                  #'identity))
           (list (org-element-property :begin elt)
                 "heading has no statistics cookie")))))

(org-lint-add-checker 'bcc32-org-lint-statistics-cookies
  "Report missing statistics cookies for headings with TODO subitems"
  #'bcc32-org-lint-statistics-cookies
  :categories '(bcc32-org))

(defun bcc32-org-lint--skip-buffer-p (buf)
  "Return non-nil if the entire buffer BUF should not be linted."
  (buffer-local-value 'buffer-read-only buf))

;;;###autoload
(defun bcc32-org-lint-agenda-buffers ()
  "Run `org-lint' in all org agenda files, erroring at the first lint."
  (interactive)
  (dolist-with-progress-reporter (buf (cl-remove-if #'bcc32-org-lint--skip-buffer-p (org-buffer-list 'agenda)))
      "Linting..."
    (let ((display-buffer-overriding-action '(display-buffer-no-window (allow-no-window t))))
      (org-lint--display-reports buf org-lint--checkers))
    (when (buffer-local-value 'tabulated-list-entries (get-buffer "*Org Lint*"))
      (pop-to-buffer "*Org Lint*")
      (display-buffer buf)
      (error "Lint found errors in buffer"))))

(provide 'bcc32-org-lint)
;;; bcc32-org-lint.el ends here
