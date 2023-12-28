(defun bcc32-hard-disable-command ()
  "Raise an error and educate the user not to press those keys.

The error message contains whatever keys were used to invoke the
current command, so you can "
  (interactive)
  (user-error "Do not use %s" (key-description (this-command-keys))))
