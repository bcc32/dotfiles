(defconst bcc32-packages
  '(direnv
    evil-quickscope))

(defun bcc32/init-evil-quickscope ()
  (use-package evil-quickscope
    :config
    (global-evil-quickscope-always-mode)))

(defun bcc32/init-direnv ()
  (use-package direnv
    :defer t))
