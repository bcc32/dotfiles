;; -*- lexical-binding: t; -*-

(defun bcc32-org/goto-agenda-heading ()
  "Prompt and visit an org agenda heading."
  (interactive)
  (call-interactively
   (or (seq-find #'commandp '(consult-org-agenda
                              counsel-org-goto-all))
       (error "No command available to prompt for org agendas"))))

(when (configuration-layer/package-used-p 'org)
  (bind-keys :map spacemacs-default-map
             ("aob" . org-switchb)
             ("aog" . bcc32-org/goto-agenda-heading)))
