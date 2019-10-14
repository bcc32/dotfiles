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
 '(gofmt-command "goimports")
 '(midnight-mode t)
 '(multi-term-program "zsh")
 '(org-habit-show-habits-only-for-today nil)
 '(org-modules
   (quote
    (org-bibtex org-docview org-habit org-info org-tempo org-eshell org-man)))
 '(rust-format-on-save t)
 '(safe-local-variable-values (quote ((bcc32/ocamlformat-on-save-mode . t))))
 '(solarized-distinct-doc-face t)
 '(solarized-use-more-italic t)
 '(spaceline-info-mode t)
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
