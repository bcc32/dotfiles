;;; bcc32-fix.el --- Workarounds for bugs in Emacs or packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Aaron Zeng

;; Author: Aaron Zeng <me@bcc32.com>
;; Keywords: maint

;;; Commentary:

;;

;;; Code:

;;;###autoload
(defun bcc32-fix-apply-all ()
  "Apply all workarounds."
  (with-eval-after-load 'smartparens
    (bcc32-fix-smartparens)))

(provide 'bcc32-fix)
;;; bcc32-fix.el ends here
