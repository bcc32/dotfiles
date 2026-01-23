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
      "ic" #'bcc32-org-ensure-custom-id
      "u"  #'bcc32-org-bump-task-counter)
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
  (with-eval-after-load 'org-agenda
    (keymap-set org-agenda-mode-map "a" 'undefined)) ;avoid accidental archiving
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate)
    (set-face-attribute 'org-headline-done nil :strike-through t)
    ;; This covers capture buffers too
    (add-to-list 'display-buffer-alist
                 '((and (derived-mode . org-mode)
                        (lambda (buffer-or-name _action)
                          (buffer-base-buffer (get-buffer buffer-or-name))))
                   display-buffer-in-side-window
                   (side . bottom)
                   (dedicated . t)))))
