;; -*- lexical-binding: t; -*-

(defconst bcc32-org-packages
  '((bcc32-org :location (recipe :fetcher local))
    org))

(defun bcc32-org//cleanup-before-save-hook ()
  (when (derived-mode-p 'org-mode)
    (bcc32-org-cleanup)))

(defun bcc32-org/init-bcc32-org ()
  (use-package bcc32-org
    :defer t
    :commands (bcc32-org-cleanup
               bcc32-org-commit-and-push-all
               bcc32-org-ensure-custom-id)
    :init
    (spacemacs/set-leader-keys
      "gy" #'bcc32-org-commit-and-push-all)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "ic" #'bcc32-org-ensure-custom-id)
    (with-eval-after-load 'org-agenda
      (add-to-list 'org-agenda-bulk-custom-functions '(?x bcc32-org-agenda-babel-execute-subtree-and-done)))
    :hook (before-save . bcc32-org//cleanup-before-save-hook)
    :hook (org-mode . bcc32-org--auto-ingest-init-org-hook)
    :hook (org-mode . org-table-header-line-mode)
    :hook (org-todo-repeat . bcc32-org-todo-repeat-maybe-skip-weekends)
    :hook (org-agenda-finalize . bcc32-org-set-default-directory-to-todo))
  ;; This is honestly redundant due to the org-mode-hook defined above, but it
  ;; doesn't hurt, and it's clearer what the intention of this package is.
  (with-eval-after-load 'org
    (require 'bcc32-org)))

(defun bcc32-org/post-init-org ()
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate)
    (set-face-attribute 'org-headline-done nil :strike-through t)))
