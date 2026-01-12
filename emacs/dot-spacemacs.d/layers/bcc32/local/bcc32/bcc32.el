;;; bcc32.el --- Miscellaneous commands for Elisp development  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Aaron Zeng

;; Author: Aaron Zeng <z@bcc32.com>
;; Keywords: lisp, tools
;; Version: 0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/bcc32/dotfiles

;;; Commentary:

;; Some miscellaneous utility commands for Elisp development.

;;; Code:

(require 'cus-edit)

(defun bcc32-custom-variable-mismatch-p (var)
  "Return non-nil if VAR's default value does not match its customization type."
  (when-let* (((custom-variable-p var))
              (type (custom-variable-type var)))
    (let ((inhibit-message t))
      (custom-load-symbol var))
    (let (conv value)
      (or (condition-case nil
              (ignore (setq conv (widget-convert type)))
            (error 'invalid-type))
          (condition-case nil
              (ignore (setq value (default-value var)))
            (error 'error-evaluating-default-value))
          (condition-case nil
              (ignore (widget-apply conv :match value))
            (error 'mismatch))))))

;;;###autoload
(defun bcc32-report-bad-custom-variables ()
  "Display a report of custom variables with bad custom types."
  (interactive)
  (let* ((buf (generate-new-buffer "*bad custom variables*"))
         had-results
         (obarray-size (cl-loop for _ being the symbols count t))
         (progress (make-progress-reporter "Scanning obarray... " 0 obarray-size))
         (i 0))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (cl-loop for sym being the symbols
               do (cl-incf i)
               do (progress-reporter-update progress i)
               for result = (bcc32-custom-variable-mismatch-p sym)
               unless (string-prefix-p "lsp-" (symbol-name sym))
               when result
               do
               (with-silent-modifications
                 (print `(,sym ,result) (current-buffer)))
               (setq had-results t)))
    (progress-reporter-done progress)
    (if had-results
        (display-buffer buf)
      (kill-buffer buf))))

(require 'autorevert)

(defun bcc32-enable-auto-revert-debugging ()
  "Enable some debugging advice and command hooks to find some auto-revert issues."
  (interactive)
  (setq auto-revert-debug t)
  (setq auto-revert-verbose t)

  (add-hook 'post-command-hook #'bcc32-auto-revert-warn-if-problem-post-command)

  ;; I think this is the only function that may remove things from `auto-revert--buffer-by-watch-descriptor'
  (advice-add 'auto-revert-notify-rm-watch :after #'bcc32-auto-revert-ensure-all-buffers-accounted-for))

(defun bcc32-auto-revert-warn-if-problem-post-command ()
  "Warn if there is an auto-revert problem in the current buffer.

This is used as a `post-command-hook'."
  (when-let* ((desc auto-revert-notify-watch-descriptor))
    (unless (assoc desc auto-revert--buffer-by-watch-descriptor)
      (delay-warning
       'bcc32
       (format-message "In buffer %S, notify-watch descriptor %S is absent from global list"
                       (current-buffer)
                       desc)))))

(defun bcc32-auto-revert-ensure-all-buffers-accounted-for (&rest _)
  "Error if any buffers' notify-watch descriptor is not in the alist."
  (cl-loop for (desc . buf) in auto-revert--buffer-by-watch-descriptor
           unless (buffer-base-buffer buf)
           do
           (cl-assert (buffer-live-p buf) t)
           (cl-assert
            (equal desc
                   (buffer-local-value 'auto-revert-notify-watch-descriptor buf))
            t))

  (cl-loop for buf being the buffers
           unless (buffer-base-buffer buf)
           for desc = (buffer-local-value 'auto-revert-notify-watch-descriptor buf)
           when desc
           do
           (cl-assert
            (let ((buf2 (alist-get desc auto-revert--buffer-by-watch-descriptor)))
              (eq buf buf2)))))

(provide 'bcc32)
;;; bcc32.el ends here
