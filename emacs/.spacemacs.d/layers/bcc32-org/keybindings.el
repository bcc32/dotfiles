(when (configuration-layer/package-used-p 'org)
  (bind-keys :map spacemacs-default-map
             ("aob" . org-switchb)
             ("aog" . counsel-org-goto-all)))
