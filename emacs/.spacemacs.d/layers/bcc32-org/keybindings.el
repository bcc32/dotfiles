(when (configuration-layer/package-used-p 'org)
  (spacemacs/set-leader-keys
    "aob" 'org-switchb
    "aog" 'counsel-org-goto-all))
