(when (configuration-layer/package-used-p 'org)
  (spacemacs/set-leader-keys
    "aob" 'org-switchb
    "aog" 'counsel-org-goto-all)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "St" 'bcc32-org-sort-by-closed
    "Sa" 'bcc32-org-sort-entire-agenda))
