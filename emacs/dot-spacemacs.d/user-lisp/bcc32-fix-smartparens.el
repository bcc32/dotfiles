;;; bcc32-fix-smartparens.el --- Workarounds for Smartparens  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Aaron Zeng

;; Author: Aaron Zeng <me@bcc32.com>
;; Keywords: maint

;;; Commentary:

;;

;;; Code:

(require 'smartparens)

;;; https://github.com/Fuco1/smartparens/issues/1036

(defun bcc32-fix-smartparens-1036-1 ()
  "Work around Fuco1/smartparens#1036.

`smartparens-strict-mode' has the wrong behavior for
`eval-expression'.

Intended to be added to `eval-expression-minibuffer-setup-hook'."
  (sp-local-pair 'eval-expression-minibuffer "'" nil :actions nil)
  (sp-local-pair 'eval-expression-minibuffer "`" nil :actions nil)
  (sp-update-local-pairs 'eval-expression-minibuffer))

(defun bcc32-fix-smartparens-1036 ()
  "Work around Fuco1/smartparens#1036."
  (add-hook 'eval-expression-minibuffer-setup-hook #'bcc32-fix-smartparens-1036-1))

;;;###autoload
(defun bcc32-fix-smartparens ()
  "Apply workarounds for smartparens."
  (bcc32-fix-smartparens-1036))

(provide 'bcc32-fix-smartparens)
;;; bcc32-fix-smartparens.el ends here
