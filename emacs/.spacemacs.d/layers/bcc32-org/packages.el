(defconst bcc32-org-packages
  '((bcc32-org :location (recipe :fetcher local))
    flycheck
    org))

(defun bcc32-org//cleanup-before-save-hook ()
  (when (derived-mode-p 'org-mode)
    (bcc32-org-cleanup)))

(defun bcc32-org/init-bcc32-org ()
  (use-package bcc32-org
    :defer t
    :hook (before-save . bcc32-org//cleanup-before-save-hook)
    :hook (org-mode . bcc32-org--auto-ingest-init-org-hook)
    :hook (org-todo-repeat . bcc32-org-todo-repeat-maybe-skip-weekends)))

(defun bcc32-org/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (require 'bcc32-org-flycheck)))

(defun bcc32-org/post-init-org ()
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate)
    (set-face-attribute 'org-headline-done nil :strike-through t)))
