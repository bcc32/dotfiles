;;; funcs.el --- Commands and functions for bcc32 layer  -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(declare-function dired-do-create-files "dired-aux")

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
  (call-process-shell-command
   (format "cp --reflink=always -r -T %s %s"
           (shell-quote-argument file1) (shell-quote-argument file2))))

(defun bcc32-hard-disable-command ()
  "Raise an error and educate the user not to press those keys.

The error message contains whatever keys were used to invoke the
current command, so you can simply bind a key to this command and
it will display the right message, e.g.:

\(bind-key \"<f1>\" #\\='bcc32-hard-disable-command)"
  (interactive)
  (user-error "Do not use %s" (key-description (this-command-keys))))
