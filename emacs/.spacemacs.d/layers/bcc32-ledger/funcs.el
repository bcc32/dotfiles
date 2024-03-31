;;; funcs.el --- Commands and functions for bcc32-ledger layer  -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(require 'thunk)

(defvar ledger-commoditized-amount-regexp)
(defvar ledger-iso-date-regexp)
(defvar ledger-iterate-regexp)
(defvar ledger-post-line-regexp)
(defvar ledger-reconcile-default-commodity)
(defvar ledger-regex-iterate-group-code)
(defvar ledger-regex-iterate-group-payee)
(defvar ledger-regex-xact-line-group-code)
(defvar ledger-xact-line-regexp)

(declare-function ledger-navigate-beginning-of-xact "ledger-navigate")
(declare-function ledger-navigate-end-of-xact       "ledger-navigate")
(declare-function ledger-next-account               "ledger-post")
(declare-function ledger-next-amount                "ledger-post")
(declare-function ledger-post-align-xact            "ledger-post")
(declare-function ledger-regex-iterate-code         "ledger-regex" t t)
(declare-function ledger-remove-effective-date      "ledger-mode")
(declare-function ledger-toggle-current             "ledger-state")
(declare-function ledger-toggle-current-posting     "ledger-state")

(defconst bcc32--ledger-posting-effective-date-regexp
  (thunk-delay (rx ";" (one-or-more space) "[=" (group (regexp ledger-iso-date-regexp)) "]"))
  "Matches a comment containing an effective date for a posting.

This regexp is defined as a lazy thunk, so `thunk-force' must be
used to actually use it.")

;; TODO: This should use a file specifying some regexps like
;; `bcc32-ledger-accounts-exclude-function' does.
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

(defun bcc32-ledger-reset-xact-state ()
  "Remove effective dates and states from this xact."
  (interactive)
  (save-excursion
    (ledger-navigate-beginning-of-xact)
    (unless (looking-at ledger-xact-line-regexp)
      (error "Not at an xact"))

    ;; Remove code and surrounding ()
    (when-let ((code-beginning (match-beginning ledger-regex-xact-line-group-code)))
      ;; include the leading (
      (cl-decf code-beginning)
      ;; include the trailing ) and any whitespace
      (save-excursion
        (goto-char (1+ (match-end ledger-regex-xact-line-group-code)))
        (skip-syntax-forward "-")
        (delete-region code-beginning (point))))

    ;; Remove xact effective date
    (ledger-remove-effective-date)

    (save-restriction
      (narrow-to-region (point) (save-excursion (ledger-navigate-end-of-xact)))

      ;; Make all postings uncleared; this also removes xact state marker
      (save-excursion
        (while (< (point) (point-max))
          (forward-line)
          (when (looking-at ledger-post-line-regexp)
            (while (ledger-toggle-current-posting)))))

      ;; FIXME: This doesn't work for things like "[2024-03-01=2024-03-02]"
      ;; Remove effective dates specified in comments
      (let ((regexp (thunk-force bcc32--ledger-posting-effective-date-regexp)))
        (while (re-search-forward regexp nil t)
          (delete-region (match-beginning 0) (match-end 0)))))))

;;; funcs.el ends here
