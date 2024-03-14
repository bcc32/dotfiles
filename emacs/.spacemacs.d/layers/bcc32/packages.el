;; -*- lexical-binding: t; -*-

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
  (defvar flycheck-checkers)
  (defvar spacemacs-ledger-mode-map)

  (with-eval-after-load 'ledger-mode
    (defvar ledger-mode-map)
    ;; Emacs 28 fixes a bug where newline-and-indent incorrectly indents the
    ;; original line, but I actually like this behavior for ledger-mode.
    (bind-key "RET" #'reindent-then-newline-and-indent ledger-mode-map))

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
             ("j" . bcc32-ledger-promote-effective-date)
             ("y" . bcc32-ledger-yank-code))

  (add-hook 'ledger-mode-hook #'turn-off-auto-fill))

(defun bcc32/init-mode-line-bell ()
  (use-package mode-line-bell
    :config
    (setq mode-line-bell-flash-time 0.2)
    (mode-line-bell-mode)))

(defun bcc32/post-init-pocket-reader ())
