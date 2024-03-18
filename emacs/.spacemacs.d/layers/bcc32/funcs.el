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

(defvar ledger-commoditized-amount-regexp)
(defvar ledger-iso-date-regexp)
(defvar ledger-iterate-regexp)
(defvar ledger-reconcile-default-commodity)
(defvar ledger-regex-iterate-group-code)
(defvar ledger-regex-iterate-group-payee)

(declare-function ledger-navigate-beginning-of-xact "ledger-navigate")
(declare-function ledger-navigate-end-of-xact       "ledger-navigate")
(declare-function ledger-next-account               "ledger-post")
(declare-function ledger-next-amount                "ledger-post")
(declare-function ledger-regex-iterate-code         "ledger-regex" t t)
(declare-function ledger-toggle-current             "ledger-state")
(declare-function ledger-post-align-xact            "ledger-post")

(defconst bcc32--ledger-posting-effective-date-regexp
  (thunk-delay (rx ";" (one-or-more space) "[=" (group (regexp ledger-iso-date-regexp)) "]"))
  "Matches a comment containing an effective date for a posting.

This regexp is defined as a lazy thunk, so `thunk-force' must be
used to actually use it.")

(defun bcc32-ledger-should-insert-effective-date ()
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

(defun bcc32-ledger-fill-in-default-commodity ()
  "Add the default commodity to each posting if none is specified.

Prefix commodity symbols are not implemented."
  (interactive)
  (let ((end (save-excursion (ledger-navigate-end-of-xact) (point-marker))))
    (set-marker-insertion-type end t)
    (save-excursion
      (ledger-navigate-beginning-of-xact)
      (while-let ((amount-width (ledger-next-amount end)))
        (unless (looking-at-p ledger-commoditized-amount-regexp)
          (forward-char amount-width)
          (insert " " ledger-reconcile-default-commodity)))))
  (ledger-post-align-xact (point)))

(defun bcc32-ledger-promote-effective-date ()
  "Move the effective date for a posting in this transaction to the transaction.

Error if there are multiple postings in this transaction with
effective dates."
  (interactive)
  (let ((effective-date (thunk-force bcc32--ledger-posting-effective-date-regexp))
        (end (ledger-navigate-end-of-xact)))
    (ledger-navigate-beginning-of-xact)
    (unless (re-search-forward effective-date end t)
      (user-error "No effective date in transaction"))
    (when (re-search-forward effective-date end t)
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

(defvar-local bcc32-ledger--accounts-list-in-buffer-cache nil)
(define-advice ledger-accounts-list-in-buffer (:around (f &rest args) bcc32-ledger--cache)
  "Cache the result of `ledger-accounts-list-in-buffer'.

The result doesn't change significantly, or frequently, enough to
recompute it."
  (or bcc32-ledger--accounts-list-in-buffer-cache
      (setq bcc32-ledger--accounts-list-in-buffer-cache
            (apply f args))))

(defvar-local bcc32-ledger--excluded-accounts-regexp nil)
(defun bcc32-ledger--load-excluded-accounts ()
  "Load the list of excluded account regexps from closed-accounts.txt."
  (or bcc32-ledger--excluded-accounts-regexp
      (setq bcc32-ledger--excluded-accounts-regexp
            (let (regexps)
              (with-demoted-errors "Could not load closed accounts list: %S"
                (with-temp-buffer
                  (insert-file-contents "closed-accounts.txt")
                  (while (< (point) (point-max))
                    (let ((regexp (buffer-substring-no-properties (point) (line-end-position))))
                      (push (pcre-to-elisp regexp) regexps)
                      (forward-line)))))
              (rx-to-string
               `(or ,@(mapcar (lambda (r) `(regexp ,r)) regexps)))))))

(defun bcc32-ledger-accounts-exclude-function (account)
  "Setting for `ledger-accounts-exclude-function'.

Return non-nil if ACCOUNT should be omitted from completion."
  (bcc32-ledger--load-excluded-accounts)
  (string-match-p bcc32-ledger--excluded-accounts-regexp (car account)))

;;; funcs.el ends here
