;; -*- lexical-binding: t; -*-

(defconst bcc32-packages
  '((advent-of-code :location (recipe :fetcher github
                                      :repo "bcc32/advent-of-code"
                                      :files ("elisp/advent-of-code.el")))
    debbugs
    envrc
    evil-quickscope
    (explain-pause-mode :location (recipe :fetcher github
                                          :repo "lastquestion/explain-pause-mode"))
    (helm-make :location (recipe :fetcher github
                                 :repo "bcc32/helm-make"
                                 :branch "try-nproc-on-darwin"))
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

(defun bcc32/init-envrc ()
  (use-package envrc
    :init (envrc-global-mode)))

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
    (setq mode-line-bell-flash-time 0.2)
    (mode-line-bell-mode)))

(defun bcc32/post-init-pocket-reader ())
