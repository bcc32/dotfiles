(defconst bcc32-packages
  '((advent-of-code :location (recipe :fetcher github
                                      :repo "bcc32/advent-of-code"
                                      :files ("elisp/advent-of-code.el")))
    debbugs
    direnv
    evil-quickscope
    (explain-pause-mode :location (recipe :fetcher github
                                          :repo "lastquestion/explain-pause-mode"))
    (helm-make :location (recipe :fetcher github
                                 :repo "bcc32/helm-make"
                                 :branch "try-nproc-on-darwin"))
    ledger-mode
    mode-line-bell
    (pocket-reader :location (recipe :fetcher github
                                     :repo "bcc32/pocket-reader.el"
                                     :branch "working"))))

(defun bcc32/init-advent-of-code ()
  (use-package advent-of-code
    :defer t))

(defun bcc32/init-debbugs ()
  (use-package debbugs
    :defer t))

(defun bcc32/init-direnv ()
  (use-package direnv
    :defer t))

(defun bcc32/init-evil-quickscope ()
  (use-package evil-quickscope
    :config
    (global-evil-quickscope-always-mode)))

(defun bcc32/init-explain-pause-mode ()
  (use-package explain-pause-mode
    :config
    (explain-pause-mode)
    (setf (cadr (assoc 'explain-pause-mode minor-mode-alist)) "")))

(defun bcc32/post-init-helm-make ())

(defun bcc32/post-init-ledger-mode ()
  (with-eval-after-load 'ledger-mode
    (defvar flycheck-checkers)
    (defvar ledger-iso-date-regexp)
    (defvar ledger-iterate-regexp)
    (defvar ledger-mode-map)
    (defvar ledger-regex-iterate-group-code)
    (defvar ledger-regex-iterate-group-payee)
    (declare-function ledger-navigate-beginning-of-xact "ledger-navigate")
    (declare-function ledger-navigate-end-of-xact       "ledger-navigate")
    (declare-function ledger-next-account               "ledger-post")
    (declare-function ledger-regex-iterate-code         "ledger-regex" t t)
    (declare-function ledger-toggle-current             "ledger-state")
    (defconst bcc32/ledger-posting-effective-date-regexp
      (rx ";" (one-or-more space) "[=" (group (regexp ledger-iso-date-regexp)) "]")
      "A comment containing an effective date for a posting.")

    ;; Emacs 28 fixes a bug where newline-and-indent incorrectly indents the
    ;; original line, but I actually like this behavior for ledger-mode.
    (bind-key "RET" #'reindent-then-newline-and-indent ledger-mode-map)

    (defun bcc32-ledger-should-insert-effective-date ()
      (let ((end (save-excursion (ledger-navigate-end-of-xact) (point)))
            xact-accounts
            account)
        (beginning-of-line)
        (when (ledger-next-account end)
          (setq account (match-string 1)))
        (ledger-navigate-beginning-of-xact)
        (while (ledger-next-account end)
          (push (match-string 1) xact-accounts))
        (not (or (member account '("Assets:Cash App"
                                   "Assets:Cash:Wallet"
                                   "Assets:eBay:Pending Payouts"
                                   "Assets:Prepaid Expenses:Uber Cash"
                                   "Assets:Venmo"))
                 (seq-some (lambda (account) (string-match-p (rx bos "Income:Work:") account))
                           xact-accounts)))))

    (defun bcc32/ledger-promote-effective-date ()
      "Move the effective date for a posting in this transaction to the transaction."
      (interactive)
      (let ((end (ledger-navigate-end-of-xact)))
        (ledger-navigate-beginning-of-xact)
        (unless (re-search-forward bcc32/ledger-posting-effective-date-regexp end t)
          (error "No effective date in transaction"))
        (when (re-search-forward bcc32/ledger-posting-effective-date-regexp end t)
          (error "Multiple effective dates in transaction"))
        (let ((effective-date (match-string 1)))
          (delete-region (match-beginning 0) (match-end 0))
          (ledger-navigate-beginning-of-xact)
          (re-search-forward ledger-iso-date-regexp)
          (insert "=" effective-date)
          (ledger-toggle-current))))

    (defun bcc32/ledger-yank-code ()
      "Insert a code for the current transaction from the kill ring."
      (interactive)
      (save-excursion
        (ledger-navigate-beginning-of-xact)
        (unless (looking-at ledger-iterate-regexp)
          (user-error "Not inside xact"))
        (goto-char (match-beginning ledger-regex-iterate-group-payee))
        (when (ledger-regex-iterate-code)
          (delete-region (1- (match-beginning ledger-regex-iterate-group-code))
                         (point)))
        (let ((code (current-kill 0)))
          (insert (format "(%s) " (string-trim code))))))

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

    (cl-pushnew 'bcc32-ledger-lint flycheck-checkers))

  (bind-keys :map spacemacs-ledger-mode-map
             ("j" . bcc32/ledger-promote-effective-date)
             ("y" . bcc32/ledger-yank-code))

  (add-hook 'ledger-mode-hook #'turn-off-auto-fill))

(defun bcc32/init-mode-line-bell ()
  (use-package mode-line-bell
    :config
    (setq mode-line-bell-flash-time 0.2)
    (mode-line-bell-mode)))

(defun bcc32/post-init-pocket-reader ())
