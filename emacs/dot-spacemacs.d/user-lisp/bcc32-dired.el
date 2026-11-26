;;; bcc32-dired.el --- Extra utilities for Dired     -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Aaron L. Zeng

;; Author: Aaron L. Zeng <me@bcc32.com>

;;; Commentary:

;;

;;; Code:

(require 'dired)

(defun bcc32-dired-do-take (&optional arg)
  "Take (cp --reflink=always) marked files into target directory.

If ARG is non-nil, take next ARG files instead."
  (interactive "P")
  (let ((was-dired-async-mode (bound-and-true-p dired-async-mode)))
    (when was-dired-async-mode
      (dired-async-mode -1))
    (dired-do-create-files 'take #'bcc32-dired-take-file "Take" arg t)
    (when was-dired-async-mode
      (dired-async-mode))))

(defun bcc32-dired-take-file (file1 file2 &optional _)
  "Internal to `bcc32-dired-do-take'.  Copy FILE1 to FILE2 with reflinks."
  (with-current-buffer (generate-new-buffer "*take file*")
    (let* ((cmd (format "cp --reflink=always --preserve=timestamps -r -T %s %s"
                        (shell-quote-argument file1) (shell-quote-argument file2)))
           (ret (call-process-shell-command cmd nil t)))
      (unless (eq 0 ret)
        (display-buffer (current-buffer))
        (insert (format "%s exited with status %s\n" cmd ret))
        (error "Take failed")))))

(provide 'bcc32-dired)
;;; bcc32-dired.el ends here
