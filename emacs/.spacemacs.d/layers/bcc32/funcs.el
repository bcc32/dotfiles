;;; funcs.el --- Commands and functions for bcc32 layer  -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'thunk)

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



;;; Ledger customizations

(defvar ledger-iso-date-regexp)
(defvar ledger-iterate-regexp)
(defvar ledger-regex-iterate-group-code)
(defvar ledger-regex-iterate-group-payee)

(declare-function ledger-navigate-beginning-of-xact "ledger-navigate")
(declare-function ledger-navigate-end-of-xact       "ledger-navigate")
(declare-function ledger-next-account               "ledger-post")
(declare-function ledger-regex-iterate-code         "ledger-regex" t t)
(declare-function ledger-toggle-current             "ledger-state")

(defconst bcc32--ledger-posting-effective-date-regexp
  (thunk-delay (rx ";" (one-or-more space) "[=" (group (regexp ledger-iso-date-regexp)) "]"))
  "A comment containing an effective date for a posting.")

(with-eval-after-load 'ledger-mode
  (setq bcc32--ledger-posting-effective-date-regexp
        (thunk-force bcc32--ledger-posting-effective-date-regexp)))

(defun bcc32--ledger-should-insert-effective-date ()
  "Return non-nil if an effective date is required for this posting."
  (let ((end (save-excursion (ledger-navigate-end-of-xact)))
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

(defun bcc32-ledger-promote-effective-date ()
  "Move the effective date for a posting in this transaction to the transaction."
  (interactive)
  (let ((end (ledger-navigate-end-of-xact)))
    (ledger-navigate-beginning-of-xact)
    (unless (re-search-forward bcc32--ledger-posting-effective-date-regexp end t)
      (user-error "No effective date in transaction"))
    (when (re-search-forward bcc32--ledger-posting-effective-date-regexp end t)
      (user-error "Multiple effective dates in transaction"))
    (let ((effective-date (match-string 1)))
      (delete-region (match-beginning 0) (match-end 0))
      (ledger-navigate-beginning-of-xact)
      (re-search-forward ledger-iso-date-regexp)
      (insert "=" effective-date)
      (ledger-toggle-current))))

(defun bcc32-ledger-yank-code ()
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

;;; funcs.el ends here
