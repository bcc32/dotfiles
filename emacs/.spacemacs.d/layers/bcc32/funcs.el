;;; funcs.el --- Commands and functions for bcc32 layer  -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(declare-function dired-do-create-files "dired-aux")

(defun bcc32-declare-function (function)
  (interactive
   (list
    (intern (completing-read "Function: " obarray #'fboundp t nil nil (thing-at-point 'symbol)))))
  (let ((declaration (pp-to-string `(declare-function ,function
                                                      ,(file-name-base (symbol-file function 'defun))
                                                      ,(help-function-arglist function 'preserve-names)))))
    (save-excursion
      (goto-char (point-min))
      (while (pcase (read (current-buffer)) (`(require . ,_) t)))
      (backward-sexp)
      (message "%s" declaration)
      (insert declaration)
      (recursive-edit))))

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

(defun bcc32-kill-ring-save-refill-for-web (beg end)
  "Copy the region BEG to END to the kill ring, refilling the text for web use.

When pasting into web forms, newlines separate paragraphs, unlike
in Emacs where paragraphs are delimited by empty lines.  This
command fills the copied text with no newlines within paragraphs."
  (interactive "r")
  (let ((filter-buffer-substring-function
         (lambda (beg end delete)
           (let ((contents (if delete
                               (delete-and-extract-region beg end)
                             (buffer-substring beg end))))
             (with-temp-buffer
               (insert contents)
               (setq fill-column most-positive-fixnum)
               (indent-region (point-min) (point-max))
               (fill-region (point-min) (point-max))
               (buffer-string))))))
    (kill-ring-save beg end)))

(with-eval-after-load 'evil
  (defun bcc32-wrap-thunk (start end)
    "Wrap the expression in the region from START to END as a thunk.

The \"thunk\" syntax depends on the current major mode, e.g., in
Elisp buffers it makes the current region the body of a lambda
expression."
    (interactive "r")
    (cl-flet ((wrap (before after)
                (save-excursion (goto-char end) (insert after))
                (save-excursion (goto-char start) (insert before))))
      (cond
       ((derived-mode-p 'tuareg-mode) (wrap "(fun () -> " ")"))
       ((derived-mode-p 'emacs-lisp-mode) (wrap "(lambda () " ")")))))

  (evil-define-operator bcc32-make-thunk (beg end)
    "Wrap the region from BEG to END in a function taking no arguments."
    (bcc32-wrap-thunk beg end)))
