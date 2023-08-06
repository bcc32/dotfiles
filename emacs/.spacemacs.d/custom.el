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
 '(counsel-org-goto-all-outline-path-prefix 'buffer-name)
 '(dired-async-mode t)
 '(dired-async-skip-fast t)
 '(dired-omit-verbose nil)
 '(display-time-mode t)
 '(enable-recursive-minibuffers t)
 '(enable-remote-dir-locals t)
 '(epg-pinentry-mode 'loopback)
 '(eshell-visual-commands
   '("ncdu" "vi" "screen" "tmux" "top" "htop" "less" "more" "lynx" "ncftp" "mutt" "pine" "tin" "trn" "elm"))
 '(eshell-visual-subcommands '(("git" "log" "diff" "show")))
 '(evil-kill-on-visual-paste nil)
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
 '(lsp-inlay-hint-enable t)
 '(lsp-rust-analyzer-completion-add-call-parenthesis nil)
 '(magit-clone-default-directory "~/src/")
 '(magit-diff-refine-hunk t)
 '(merlin-eldoc-doc nil)
 '(merlin-eldoc-type nil)
 '(midnight-mode t)
 '(minibuffer-depth-indicate-mode t)
 '(org-agenda-dim-blocked-tasks t)
 '(org-agenda-files
   (seq-filter #'file-directory-p
               '("~/file-server/" "~/journal/" "~/src/film-metadata/" "~/src/watch-later/" "~/todo/" "~/todo/ideas/")))
 '(org-agenda-prefer-last-repeat t)
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
      "* TODO %?
%U
%a")))
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
 '(org-refile-use-outline-path 'buffer-name)
 '(org-startup-indented t)
 '(org-startup-shrink-all-tables t)
 '(org-stuck-projects '("+LEVEL<=2/!" ("TODO" "MAYBE" "INPROGRESS") nil ""))
 '(org-table-automatic-realign nil)
 '(org-todo-keywords
   '((sequence "TODO(t)" "MAYBE(m/!)" "INPROGRESS(p!)" "STALLED(a)" "BLOCKED(k@/!)" "INREVIEW(r!)" "|" "DONE(d/@)" "DELEGATED(g@)" "DEFERRED(e!)" "NOTDONE(n)")
     (type "BUG(b/!)" "CLEANUP(l/!)" "|" "FIXED(x/@)" "WONTFIX(w@/@)")
     (type "ENHANCEMENT(h/!)" "FEATURE(f/!)" "|" "RELEASED(s/@)" "CANCELED(c/@)")))
 '(org-wild-notifier-alert-time '(10 2))
 '(paradox-github-token t)
 '(pocket-reader-archive-on-open nil)
 '(projectile-ignored-project-function 'bcc32/projectile-ignored-project-function)
 '(rustic-format-trigger 'on-save)
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
     (flycheck-mode)
     (javascript-backend . lsp)
     (javascript-backend . tern)
     (javascript-backend . tide)
     (magit-todos-mode)
     (projectile-project-compilation-cmd . "cargo test -q && cargo clippy -- -Dwarnings -Dclippy::pedantic")
     (projectile-project-compilation-cmd . "cargo check && cargo clippy -- -Dwarnings -Dclippy::pedantic")
     (projectile-project-compilation-cmd . "make")
     (projectile-project-compilation-cmd . "make -j")
     (typescript-backend . lsp)
     (typescript-backend . tide)
     (vc-git-annotate-switches . "-w")
     (vc-prepare-patches-separately)))
 '(save-abbrevs 'silently)
 '(sentence-end-double-space t)
 '(sh-basic-offset 2)
 '(shfmt-arguments '("-i" "2" "-bn" "-s"))
 '(solarized-distinct-doc-face t)
 '(solarized-use-more-italic t)
 '(sql-product 'sqlite)
 '(tab-width 8)
 '(tao-theme-use-sepia nil)
 '(terminal-here-linux-terminal-command 'urxvt)
 '(terminal-here-mac-terminal-command 'iterm2)
 '(tramp-use-ssh-controlmaster-options nil t nil "NOW is set as a workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47063.  Remove after upgrading all machines to Emacs 29.1")
 '(tuareg-prettify-symbols-full t)
 '(typescript-indent-level 2)
 '(valign-fancy-bar t)
 '(vc-follow-symlinks t)
 '(what-cursor-show-names t)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :underline t))))
 '(tuareg-font-lock-attribute-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
 '(tuareg-font-lock-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
 '(tuareg-font-lock-infix-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic)))))
