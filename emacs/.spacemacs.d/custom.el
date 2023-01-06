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
 '(calendar-week-start-day 1)
 '(column-enforce-column nil)
 '(counsel-find-file-at-point nil)
 '(dired-async-mode t)
 '(dired-omit-verbose nil)
 '(display-time-mode t)
 '(enable-recursive-minibuffers t)
 '(enable-remote-dir-locals t)
 '(epg-pinentry-mode 'loopback)
 '(eshell-visual-commands
   '("ncdu" "vi" "screen" "tmux" "top" "htop" "less" "more" "lynx" "ncftp" "mutt" "pine" "tin" "trn" "elm"))
 '(eshell-visual-subcommands '(("git" "log" "diff" "show")))
 '(evil-symbol-word-search t)
 '(evil-want-Y-yank-to-eol t)
 '(fancy-battery-mode t)
 '(flycheck-emacs-lisp-initialize-packages t nil nil "Set so that ~/.spacemacs.d/init.el is checked properly")
 '(flycheck-idle-change-delay 1.0)
 '(flycheck-ledger-explicit t)
 '(flycheck-ledger-pedantic t)
 '(garbage-collection-messages t)
 '(geiser-default-implementation 'chicken)
 '(global-column-enforce-mode t)
 '(gofmt-command "goimports")
 '(grep-files-aliases
   '(("ocaml" . "*.ml *.mli *.mll *.mly")
     ("all" . "* .[!.]* ..?*")
     ("el" . "*.el")
     ("ch" . "*.[ch]")
     ("c" . "*.c")
     ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]")))
 '(grep-find-ignored-directories
   '("_build" "_opam" "SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))
 '(helm-make-nproc 0)
 '(indent-guide-global-mode t)
 '(ivy-magic-tilde nil)
 '(ivy-use-virtual-buffers nil)
 '(js-indent-level 2)
 '(ledger-accounts-file (expand-file-name "declarations.ldg" "~/journal"))
 '(ledger-default-date-format ledger-iso-date-format)
 '(ledger-reconcile-default-commodity "USD")
 '(ledger-reconcile-insert-effective-date 'bcc32-ledger-should-insert-effective-date)
 '(ledger-report-use-header-line t)
 '(ledger-reports
   '(("cleared" "%(binary) -f %(ledger-file) bal --current -C %(account)")
     ("debts" "ledger register --by-payee 'Personal Debts'")
     ("reconcile" "%(binary) -f %(ledger-file) reg --current --effective --sort '(X ? 0 : 1), d' -d \"!X || d>=[90 days ago]\" \"/^%(account)$/\" | tac")
     ("reimbursement" "%(binary) -f %(ledger-file) --group-by payee reg -U '^Assets:Reimbursable Expenses'")
     ("uncleared" "%(binary) -f %(ledger-file) reg --current -U --group-by account '^Assets' '^Equity' '^Liabilities'")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(lsp-enable-suggest-server-download nil)
 '(magit-clone-default-directory "~/src/")
 '(midnight-mode t)
 '(minibuffer-depth-indicate-mode t)
 '(mode-line-bell-flash-time 0.2)
 '(mode-line-bell-mode t)
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-files '("~/journal/" "~/todo/" "~/todo/ideas/"))
 '(org-agenda-prefer-last-repeat t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span 'day)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-agenda-todo-ignore-time-comparison-use-seconds t)
 '(org-appear-autolinks nil)
 '(org-attach-commit nil t nil "This variable is obsolete but has effect in some older versions of org-mode.")
 '(org-capture-templates
   '(("g" "Groceries" entry
      (file+olp "~/todo/shopping-list.org" "Groceries")
      "* TODO %? %^g")
     ("t" "Todo" entry
      (file "")
      "* TODO %?
%U
%a")))
 '(org-clock-history-length 35)
 '(org-clock-persist t)
 '(org-default-notes-file "~/todo/refile.org")
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends '(ascii gfm html icalendar latex md org texinfo))
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
 '(org-refile-use-outline-path 'buffer-name)
 '(org-startup-indented t)
 '(org-startup-shrink-all-tables t)
 '(org-stuck-projects '("+LEVEL<=2/!" ("TODO" "MAYBE" "INPROGRESS") nil ""))
 '(org-todo-keywords
   '((sequence "TODO(t)" "MAYBE(m/!)" "INPROGRESS(p!)" "STALLED(a)" "BLOCKED(k@/!)" "INREVIEW(r!)" "|" "DONE(d/@)" "DELEGATED(g@)" "DEFERRED(e!)" "NOTDONE(n)")
     (type "BUG(b/!)" "CLEANUP(l/!)" "|" "FIXED(x/@)" "WONTFIX(w@/@)")
     (type "ENHANCEMENT(h/!)" "FEATURE(f/!)" "|" "RELEASED(s/@)" "CANCELED(c/@)")))
 '(org-wild-notifier-alert-time '(10 2))
 '(paradox-github-token t)
 '(pocket-reader-archive-on-open nil)
 '(projectile-ignored-project-function 'bcc32/projectile-ignored-project-function)
 '(rust-format-on-save t)
 '(rust-format-show-buffer nil)
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
 '(save-abbrevs 'silently)
 '(sentence-end-double-space t)
 '(sh-basic-offset 2)
 '(shfmt-arguments '("-i" "2" "-bn" "-s"))
 '(solarized-distinct-doc-face t)
 '(solarized-use-more-italic t)
 '(spaceline-info-mode t)
 '(sql-product 'sqlite)
 '(tab-width 8)
 '(tao-theme-use-sepia nil)
 '(terminal-here-linux-terminal-command 'urxvt)
 '(terminal-here-mac-terminal-command 'iterm2)
 '(tramp-use-ssh-controlmaster-options nil t nil "NOW is set as a workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47063")
 '(tuareg-prettify-symbols-full t)
 '(typescript-indent-level 2)
 '(valign-fancy-bar t)
 '(vc-follow-symlinks t)
 '(what-cursor-show-names t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :underline t))))
 '(tuareg-font-lock-attribute-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
 '(tuareg-font-lock-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
 '(tuareg-font-lock-infix-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic)))))
