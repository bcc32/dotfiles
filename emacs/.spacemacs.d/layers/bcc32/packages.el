;; -*- lexical-binding: t; -*-

(defconst bcc32-packages
  '((advent-of-code :location (recipe :fetcher github
                                      :repo "bcc32/advent-of-code"
                                      :files ("elisp/advent-of-code.el")))
    (bcc32 :location local)
    corfu
    debbugs
    envrc
    evil
    evil-quickscope
    (explain-pause-mode :location (recipe :fetcher github
                                          :repo "lastquestion/explain-pause-mode"))
    (helm-make :location (recipe :fetcher github
                                 :repo "bcc32/helm-make"))
    mode-line-bell
    vertico))

(defun bcc32/init-advent-of-code ()
  (use-package advent-of-code
    :defer t))

(defun bcc32/init-bcc32 ()
  (use-package bcc32
    :commands (bcc32-enable-auto-revert-debugging
               bcc32-report-bad-custom-variables)
    :defer t
    :init
    (when bcc32-enable-auto-revert-debugging-at-startup
      (bcc32-enable-auto-revert-debugging))))

(defun bcc32/init-corfu ()
  (use-package corfu
    :defer t
    :init (global-corfu-mode)
    :config
    (setopt corfu-auto t)))

(defun bcc32/init-debbugs ()
  (use-package debbugs
    :defer t))

(defun bcc32/init-envrc ()
  (use-package envrc
    :init (envrc-global-mode)))

(defun bcc32/post-init-evil ()
  (with-eval-after-load 'evil
    (evil-define-key* '(visual) prog-mode-map "t" #'bcc32-wrap-thunk)))

(defun bcc32/init-evil-quickscope ()
  (use-package evil-quickscope
    :config
    (global-evil-quickscope-always-mode)))

(defun bcc32/init-explain-pause-mode ()
  (use-package explain-pause-mode
    :defer t
    :init
    (when bcc32-enable-explain-pause-at-startup
      (explain-pause-mode))
    :config
    (setf (cadr (assoc 'explain-pause-mode minor-mode-alist)) "")))

(defun bcc32/post-init-helm-make ())

(defun bcc32/init-mode-line-bell ()
  (use-package mode-line-bell
    :config
    (mode-line-bell-mode)))

(defun bcc32/post-init-vertico ()
  (with-eval-after-load 'vertico
    (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)))
