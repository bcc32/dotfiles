;;; packages.el --- bcc32-ledger layer packages file for Spacemacs.

;;; Commentary:

;;; Code:

(defconst bcc32-ledger-packages
  '((ledger-mode :location (recipe :fetcher github
                                   :repo "bcc32/ledger-mode"
                                   :branch "bcc32"
                                   :files ("ledger-*.el" "doc/*.texi")))))

(defun bcc32-ledger/post-init-ledger-mode ()
  (defvar flycheck-checkers)
  (defvar spacemacs-ledger-mode-map)

  (with-eval-after-load 'ledger-mode
    (defvar ledger-mode-map)
    ;; Emacs 28 fixes a bug where newline-and-indent incorrectly indents the
    ;; original line, but I actually like this behavior for ledger-mode.
    (bind-key "RET" #'reindent-then-newline-and-indent ledger-mode-map))

  ;; FIXME: ledger already defines a flymake checker, can we just reuse it for
  ;; flycheck?
  (flycheck-define-command-checker 'bcc32-ledger-lint
    "Lint by running `bin/check.sh' with no arguments."
    :command '("bin/check.sh" source-inplace)
    :error-patterns
    '((error line-start "sort: " (file-name) ":" line ":" (message))
      (error line-start (file-name) ":" line ":"
             (message (one-or-more (or not-newline "\n " (seq "\n" line-end)))))
      (error line-start "While parsing file \"" (file-name) "\", line " line ":"
             (message (+? anychar) line-start "Error:" (one-or-more nonl) line-end)))
    :modes 'ledger-mode
    :next-checkers '(ledger))

  (cl-pushnew 'bcc32-ledger-lint flycheck-checkers)

  (bind-keys :map spacemacs-ledger-mode-map
             ("d" . bcc32-ledger-append-adjustments-null-posting)
             ("j" . bcc32-ledger-promote-effective-date)
             ("u" . bcc32-ledger-fill-in-default-commodity)
             ("y" . bcc32-ledger-yank-code))

  (add-hook 'ledger-mode-hook #'turn-off-auto-fill)
  (add-hook 'ledger-reconcile-mode-hook #'ledger-reconcile-display-balance-in-header-mode)

  (define-advice ledger-copy-transaction-at-point (:after (&rest _) reset-xact-state)
    "Make sure copied xact has clean state."
    (bcc32-ledger-reset-xact-state)))
