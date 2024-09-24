(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(advent-of-code-cookie-jar "~/src/advent-of-code/cookies.txt")
 '(advent-of-code-email "me@bcc32.com")
 '(after-save-hook '(executable-make-buffer-file-executable-if-script-p))
 '(browse-url-browser-function 'bcc32/browse-url-on-ssh-client-if-exists)
 '(calc-context-sensitive-enter t)
 '(calc-settings-file
   (expand-file-name "calc-settings.el" dotspacemacs-directory))
 '(calendar-latitude 40.65)
 '(calendar-longitude -73.95)
 '(calendar-mark-holidays-flag t)
 '(calendar-week-start-day 1)
 '(column-enforce-column nil)
 '(completions-detailed t)
 '(corfu-auto-delay 0.5)
 '(counsel-org-goto-all-outline-path-prefix 'buffer-name)
 '(dired-async-mode t)
 '(dired-async-skip-fast t)
 '(dired-auto-revert-buffer t)
 '(dired-omit-verbose nil)
 '(dired-recursive-copies 'always)
 '(dired-vc-rename-file t)
 '(display-time-mode t)
 '(editorconfig-mode-lighter "")
 '(eglot-autoshutdown t)
 '(eglot-confirm-server-initiated-edits nil)
 '(enable-recursive-minibuffers t)
 '(enable-remote-dir-locals t)
 '(epg-pinentry-mode 'loopback)
 '(eshell-visual-commands
   '("ncdu" "vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "ncftp" "mutt" "pine" "tin" "trn" "elm"))
 '(eshell-visual-subcommands '(("git" "log" "diff" "show")))
 '(evil-escape-excluded-major-modes '(term-mode))
 '(evil-ex-visual-char-range t)
 '(evil-kill-on-visual-paste nil)
 '(evil-symbol-word-search t)
 '(evil-want-Y-yank-to-eol t)
 '(evil-want-fine-undo t)
 '(explicit-shell-file-name "zsh")
 '(fancy-battery-mode t)
 '(flycheck-emacs-lisp-initialize-packages t nil nil "Set so that ~/.spacemacs.d/init.el is checked properly")
 '(flycheck-emacs-lisp-load-path '("./"))
 '(flycheck-ledger-explicit t)
 '(flycheck-ledger-pedantic t)
 '(garbage-collection-messages t)
 '(geiser-default-implementation 'chicken)
 '(global-column-enforce-mode t)
 '(gofmt-command "goimports")
 '(grep-files-aliases
   '(("ml" . "*.ml *.mli *.mll *.mlt *.mly *.mdx")
     ("all" . "* .[!.]* ..?*")
     ("el" . "*.el")
     ("ch" . "*.[ch]")
     ("c" . "*.c")
     ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("am" . "Makefile.am GNUmakefile *.mk")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]")))
 '(grep-find-ignored-directories
   '("_build" "_opam" "SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))
 '(helm-make-nproc 0)
 '(help-enable-variable-value-editing t)
 '(holiday-bahai-holidays nil)
 '(imenu-max-item-length 300)
 '(indent-guide-global-mode t)
 '(initial-buffer-choice t)
 '(ivy-magic-tilde nil)
 '(ivy-use-virtual-buffers nil)
 '(js-indent-level 2)
 '(ledger-accounts-exclude-function 'bcc32-ledger-accounts-exclude-function)
 '(ledger-copy-transaction-insert-blank-line-after t)
 '(ledger-default-date-format ledger-iso-date-format)
 '(ledger-reconcile-default-commodity "USD")
 '(ledger-reconcile-insert-effective-date 'bcc32-ledger-should-insert-effective-date)
 '(ledger-report-links-beginning-of-xact nil)
 '(ledger-report-use-header-line t)
 '(ledger-reports
   '(("debts" "%(binary) -f %(ledger-file) register --by-payee 'Personal Debts'")
     ("net-worth" "%(binary) -f %(ledger-file) bal '^Assets' '^Liabilities' -X USD")
     ("reconcile" "%(binary) -f %(ledger-file) reg --current --effective --sort '(X ? 0 : 1), d' -d \"!X || d>=[90 days ago]\" \"/^%(account)$/\" | tac")
     ("reimbursement" "%(binary) -f %(ledger-file) --group-by payee reg -U '^Assets:Reimbursable Expenses'")
     ("uncleared" "%(binary) -f %(ledger-file) reg --current -U --group-by account '^Assets' '^Equity' '^Liabilities'")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(lsp-enable-suggest-server-download nil)
 '(lsp-inlay-hint-enable t)
 '(lsp-rust-analyzer-completion-add-call-parenthesis nil)
 '(lsp-solargraph-use-bundler t)
 '(magit-clone-default-directory "~/src/")
 '(magit-diff-refine-hunk t)
 '(magit-published-branches '("origin/main" "origin/master"))
 '(magit-todos-exclude-globs '("flycheck_*" "*.org_archive" ".git/"))
 '(magit-todos-update 120)
 '(merlin-eldoc-doc nil)
 '(merlin-eldoc-type nil)
 '(midnight-mode t)
 '(minibuffer-depth-indicate-mode t)
 '(mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*"))
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "offlineimap")
 '(mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:maildir . 10)
     (:from-or-to . 22)
     (:subject)))
 '(mu4e-maildir-shortcuts
   '((:maildir "/INBOX" :key 105)
     (:maildir "/TODO.Input into ledger" :key 108)))
 '(mu4e-refile-folder "/Archive")
 '(mu4e-search-skip-duplicates nil)
 '(mu4e-sent-folder "/Sent")
 '(mu4e-trash-folder "/Trash")
 '(orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefixes))
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-files
   (seq-map #'file-truename
            (seq-filter #'file-directory-p
                        '("~/file-server/" "~/journal/" "~/src/film-metadata/" "~/src/watch-later/" "~/todo/" "~/todo/ideas/"))))
 '(org-agenda-include-diary t)
 '(org-agenda-prefer-last-repeat t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span 'day)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-agenda-todo-ignore-time-comparison-use-seconds t)
 '(org-appear-autolinks nil)
 '(org-capture-templates
   '(("g" "Groceries" entry
      (file+olp "~/todo/shopping-list.org" "Groceries")
      "* TODO %? %^g")
     ("t" "Todo" entry
      (file "")
      "* TODO %?\12%U\12%a")))
 '(org-clock-history-length 35)
 '(org-clock-persist t)
 '(org-default-notes-file "~/todo/refile.org")
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii gfm html icalendar latex md org texinfo))
 '(org-extend-today-until 2)
 '(org-habit-graph-column 80)
 '(org-habit-show-habits-only-for-today nil)
 '(org-html-htmlize-output-type 'css)
 '(org-insert-heading-respect-content t)
 '(org-log-into-drawer t)
 '(org-log-redeadline 'note)
 '(org-log-reschedule 'time)
 '(org-modules
   '(ol-bibtex ol-docview org-habit ol-info org-tempo ol-eshell org-checklist ol-man))
 '(org-outline-path-complete-in-steps nil)
 '(org-pretty-entities t)
 '(org-priority-default 67)
 '(org-refile-targets '((nil :maxlevel . 10) (org-agenda-files :maxlevel . 10)))
 '(org-refile-use-cache t)
 '(org-refile-use-outline-path 'buffer-name)
 '(org-startup-folded 'nofold)
 '(org-startup-indented t)
 '(org-startup-shrink-all-tables t)
 '(org-stuck-projects '("+LEVEL<=2/!" ("TODO" "MAYBE" "INPROGRESS") nil ""))
 '(org-todo-keywords
   '((sequence "TODO(t)" "MAYBE(m/!)" "INPROGRESS(p!)" "STALLED(a)" "BLOCKED(k@/!)" "INREVIEW(r!)" "|" "DONE(d/@)" "DELEGATED(g@)" "DEFERRED(e!)" "NOTDONE(n)")
     (type "BUG(b/!)" "CLEANUP(l/!)" "|" "FIXED(x/@)" "WONTFIX(w@/@)")
     (type "ENHANCEMENT(h/!)" "FEATURE(f/!)" "|" "RELEASED(s/@)" "CANCELED(c/@)")))
 '(org-use-effective-time t)
 '(org-web-tools-pandoc-sleep-time 2.0)
 '(org-wild-notifier-alert-time '(10 2))
 '(orgit-rev-description-format "%%N (magit-rev %%R): %s")
 '(paradox-github-token t nil nil "Disable GitHub integration in Paradox")
 '(pdf-tools-installer-os "nixos")
 '(pocket-reader-archive-on-open nil)
 '(projectile-ignored-project-function 'bcc32/projectile-ignored-project-function)
 '(rustic-default-test-arguments "--quiet --benches --tests --all-features")
 '(rustic-format-trigger 'on-compile)
 '(rustic-lsp-client 'eglot)
 '(safe-local-variable-values
   '((auto-insert-alist
      (rust-mode "" "// "
                 '(setq v1
                        (read-from-minibuffer "Title: "))
                 v1 n "// "
                 '(setq v2
                        (read-from-minibuffer "URL: "))
                 v2 n n "use super::def::*;" n n _ n n "#[cfg(test)]" n "mod tests {" n "use super::*;" n "use test_case::test_case;" n n "#[test_case(args => result)]" n "fn test(args: Ty) -> Ty {" n "}" n "}" n))
     (diff-add-log-use-relative-names . t)
     (dired-guess-shell-alist-user
      ("\\.zip\\'" "../bin/import.sh"))
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (flycheck-mode)
     (javascript-backend . lsp)
     (javascript-backend . tern)
     (javascript-backend . tide)
     (lsp-rust-analyzer-diagnostics-disabled .
                                             ["unresolved-proc-macro"])
     (magit-todos-exclude-globs ".git/" ".vendor/")
     (magit-todos-mode)
     (magit-todos-rg-extra-args "--hidden")
     (org-archive-location . "archive/%s_archive::")
     (org-table-copy-increment)
     (projectile-project-compilation-cmd . "make -O -j")
     (projectile-project-compilation-cmd . "nix run 'nixpkgs#cmake' -- -S . && make && make -C test test-batch")
     (typescript-backend . lsp)
     (typescript-backend . tide)
     (vc-git-annotate-switches . "-w")
     (vc-prepare-patches-separately)
     (whitespace-style)))
 '(save-abbrevs 'silently)
 '(save-some-buffers-default-predicate 'save-some-buffers-root)
 '(send-mail-function 'smtpmail-send-it)
 '(sentence-end-double-space t)
 '(sh-basic-offset 2)
 '(shell-file-name (executable-find "bash"))
 '(shell-pop-term-shell explicit-shell-file-name nil nil "prevent Spacemacs shell layer from overriding the defcustom default")
 '(shfmt-arguments '("-i" "2" "-bn" "-s"))
 '(smtpmail-debug-info t)
 '(smtpmail-servers-requiring-authorization "smtp[.]fastmail[.]com")
 '(smtpmail-smtp-server "smtp.fastmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-smtp-user "z@bcc32.com")
 '(smtpmail-stream-type 'ssl)
 '(solarized-distinct-doc-face t)
 '(solarized-use-more-italic t)
 '(spacemacs-keep-legacy-current-buffer-delete-bindings nil)
 '(spacemacs-large-file-modes-list
   '(org-mode ledger-mode archive-mode tar-mode jka-compr git-commit-mode image-mode doc-view-mode doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode tags-table-mode fundamental-mode))
 '(sql-product 'sqlite)
 '(tab-always-indent 'complete)
 '(tab-width 8)
 '(tao-theme-use-sepia nil)
 '(terminal-here-linux-terminal-command 'urxvt)
 '(terminal-here-mac-terminal-command 'iterm2)
 '(tramp-use-ssh-controlmaster-options nil)
 '(tuareg-prettify-symbols-full t)
 '(typescript-indent-level 2)
 '(undo-tree-auto-save-history nil)
 '(user-mail-address "z@bcc32.com")
 '(vc-follow-symlinks t)
 '(what-cursor-show-names t)
 '(which-function-mode t)
 '(zoneinfo-style-world-list
   '(("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Amsterdam" "Amsterdam")
     ("Asia/Hong_Kong" "Hong Kong")
     ("Asia/Shanghai" "Shanghai")
     ("Asia/Tokyo" "Tokyo"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :underline t))))
 '(tuareg-font-lock-attribute-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
 '(tuareg-font-lock-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
 '(tuareg-font-lock-infix-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic)))))
