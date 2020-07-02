(defconst bcc32-org-packages
  '((bcc32-org :location local)
    flycheck
    org))

(defun bcc32-org//cleanup-before-save-hook ()
  (when (derived-mode-p 'org-mode)
    (bcc32-org-cleanup)))

(defun bcc32-org/init-bcc32-org ()
  (use-package bcc32-org
    :hook (before-save . bcc32-org//cleanup-before-save-hook)
    :hook (org-mode . bcc32-org--auto-ingest-init-org-hook)
    :bind (:map org-mode-map
                ("<f9>" . bcc32-org-commit-and-push-all))))

(defun bcc32-org/post-init-flycheck ()
  (with-eval-after-load 'org
    (with-eval-after-load 'flycheck
      ;; TODO: Rename this checker to include bcc32- prefix
      (flycheck-define-generic-checker 'org-lint
        "An org linter to enforce statistics cookies where appropriate."
        :start #'bcc32-org/flycheck-start
        :modes 'org-mode
        :predicate #'org-agenda-file-p)
      (add-to-list 'flycheck-checkers 'org-lint))))

(defun bcc32-org/post-init-org ()
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate)))
