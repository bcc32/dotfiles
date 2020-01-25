(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote xetex))
 '(calc-context-sensitive-enter t)
 '(calc-settings-file "~/.spacemacs.d/calc-settings.el")
 '(cperl-close-paren-offset -2 t)
 '(cperl-indent-parens-as-block t t)
 '(dired-async-mode t)
 '(enable-recursive-minibuffers t)
 '(eshell-visual-commands
   (quote
    ("ncdu" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm")))
 '(evil-want-Y-yank-to-eol t)
 '(flycheck-idle-change-delay 1.0)
 '(geiser-default-implementation (quote chicken))
 '(gofmt-command "goimports")
 '(helm-make-nproc 0)
 '(ivy-use-virtual-buffers nil)
 '(ledger-accounts-file
   (concat
    (file-name-as-directory
     (getenv "LEDGER_PATH"))
    "declarations.ldg"))
 '(ledger-reports
   (quote
    (("reconcile" "ldg-reconcile %(account)")
     ("uncleared" "%(binary) reg -U '^Assets' '^Equity' '^Liabilities'")
     ("bal" "%(binary) bal")
     ("payee" "%(binary) reg @%(payee)")
     ("account" "%(binary) reg %(account)")
     ("validate" "%(binary) source %(env-ledger-file)"))))
 '(merlin-locate-preference (quote mli))
 '(midnight-mode t)
 '(minibuffer-depth-indicate-mode t)
 '(multi-term-program "zsh")
 '(org-agenda-files (expand-file-name "agenda-files" org-directory))
 '(org-agenda-span (quote day))
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-agenda-todo-ignore-time-comparison-use-seconds t)
 '(org-attach-commit nil t nil "This variable is obsolete but has effect in some older versions of org-mode.")
 '(org-capture-templates (quote (("t" "Todo" entry (file "") "* TODO %?
%U
%a"))))
 '(org-clock-persist t)
 '(org-default-notes-file (expand-file-name "refile.org" org-directory))
 '(org-default-priority 67)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends (quote (ascii html icalendar latex md org texinfo)))
 '(org-extend-today-until 2)
 '(org-habit-show-habits-only-for-today nil)
 '(org-html-htmlize-output-type (quote css))
 '(org-insert-heading-respect-content t)
 '(org-link-abbrev-alist (quote (("att" . org-attach-expand-link))))
 '(org-log-into-drawer t)
 '(org-modules
   (quote
    (org-bibtex org-docview org-habit org-info org-tempo org-eshell org-checklist org-man)))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 10)
     (org-agenda-files :maxlevel . 10))))
 '(org-refile-use-outline-path (quote buffer-name))
 '(org-startup-indented t)
 '(org-stuck-projects
   (quote
    ("+LEVEL<=2/!"
     ("TODO" "MAYBE" "INPROGRESS")
     nil "")))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "MAYBE(m/!)" "INPROGRESS(p!)" "STALLED(a)" "BLOCKED(k@/!)" "INREVIEW(r!)" "|" "DONE(d/@)" "DELEGATED(g@)" "DEFERRED(e!)" "NOTDONE(n)")
     (type "BUG(b/!)" "CLEANUP(l/!)" "|" "FIXED(x/@)" "WONTFIX(w@/@)")
     (type "ENHANCEMENT(h/!)" "FEATURE(f/!)" "|" "RELEASED(s/@)" "CANCELED(c/@)"))))
 '(rust-format-on-save t)
 '(safe-local-variable-values (quote ((bcc32/ocamlformat-on-save-mode . t))))
 '(sentence-end-double-space t)
 '(solarized-distinct-doc-face t)
 '(solarized-use-more-italic t)
 '(spaceline-info-mode t)
 '(tab-width 8)
 '(tuareg-prettify-symbols-full t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :underline t))))
 '(tuareg-font-lock-attribute-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
 '(tuareg-font-lock-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
 '(tuareg-font-lock-infix-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic)))))
