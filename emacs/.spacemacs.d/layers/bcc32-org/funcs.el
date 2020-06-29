(when (configuration-layer/package-used-p 'flycheck)
  (defun bcc32-org//map-entry-children (proc)
    "Call PROC with point at each child of the current entry.

Collect the values returned by PROC into a list."
    (save-excursion
      (let (results)
        (when (org-goto-first-child)
          (push (funcall proc) results)
          (while (org-goto-sibling)
            (push (funcall proc) results)))
        (nreverse results))))

  (defun bcc32-org//entry-has-todo-children-p ()
    "Return non-nil if the current entry has a child with any TODO state."
    (-some #'identity (bcc32-org//map-entry-children #'org-get-todo-state)))

  (defconst bcc32-org//statistics-cookie-re
    (rx "[" (0+ digit) (or "%" (: "/" (0+ digit))) "]")
    "Pattern that matches `org-mode' statistics cookies.

Copied from `org-element-statistics-cookie-parser'.")

  (defun bcc32-org//flycheck-lint-headline-statistics-cookie (checker)
    "Lint the current headline for statistics cookies using CHECKER."
    (let ((heading (org-get-heading :no-tags :no-todo)))
      (when (and (bcc32-org//entry-has-todo-children-p)
                 (not (string-match-p bcc32-org//statistics-cookie-re heading)))
        (flycheck-error-new-at (line-number-at-pos)
                               nil
                               'error
                               "heading has no statistics cookie"
                               :checker checker))))

  (defun bcc32-org//flycheck-lint-buffer (checker)
    "Lint the current buffer for statistics cookies using CHECKER."
    (delq nil
          (org-map-entries
           (lambda ()
             (bcc32-org//flycheck-lint-headline-statistics-cookie checker)))))

  (defun bcc32-org//org-mode-use-flycheck-as-next-error-function ()
    "Use `flycheck-next-error' as the `next-error-function'.

This is so that `next-error-function' in `org-mode' buffers is
set correctly."
    (setq next-error-function 'flycheck-next-error))

  (defun bcc32-org/flycheck-start (checker callback)
    "Start linting an org buffer for syntax checker CHECKER.

CALLBACK should be a flycheck checker callback."
    (condition-case err
        (let ((errors (bcc32-org//flycheck-lint-buffer checker)))
          (funcall callback 'finished errors))
      (error (funcall callback 'errored (error-message-string err))))))
