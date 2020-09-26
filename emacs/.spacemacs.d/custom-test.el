(require 'ert)

;; Adapted from https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically
(defun custom-variable-default-value (var)
  (eval (car (get var 'standard-value))))

(ert-deftest test-eshell-visual-commands ()
  "`eshell-visual-commands' default value has not changed."
  (require 'em-term)
  (should (equal (custom-variable-default-value 'eshell-visual-commands)
                 '("vi"
                   "screen" "tmux" "top" "htop"
                   "less" "more"
                   "lynx" "links" "ncftp"
                   "mutt" "pine" "tin" "trn" "elm"))))

(ert-deftest test-eshell-visual-subcommands ()
  "`eshell-visual-subcommands' default value has not changed."
  (require 'em-term)
  (should (equal (custom-variable-default-value 'eshell-visual-subcommands)
                 nil)))

(ert-deftest test-grep-files-aliases ()
  "`grep-files-aliases' default value has not changed."
  (require 'grep)
  (should (equal (custom-variable-default-value 'grep-files-aliases)
                 '(("all" .   "* .[!.]* ..?*")
                   ("el" .    "*.el")
                   ("ch" .    "*.[ch]")
                   ("c" .     "*.c")
                   ("cc" .    "*.cc *.cxx *.cpp *.C *.CC *.c++")
                   ("cchh" .  "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
                   ("hh" .    "*.hxx *.hpp *.[Hh] *.HH *.h++")
                   ("h" .     "*.h")
                   ("l" .     "[Cc]hange[Ll]og*")
                   ("m" .     "[Mm]akefile*")
                   ("tex" .   "*.tex")
                   ("texi" .  "*.texi")
                   ("asm" .   "*.[sS]")))))

(ert-deftest test-grep-find-ignored-directories ()
  "`grep-find-ignored-directories' default value has not changed."
  (require 'grep)
  (should (equal (custom-variable-default-value 'grep-find-ignored-directories)
                 '("SCCS" "RCS" "CVS" "MCVS"
                   ".src" ".svn" ".git" ".hg" ".bzr"
                   "_MTN" "_darcs" "{arch}"))))

(ert-deftest test-org-modules ()
  "`org-modules' default value has not changed."
  (require 'org)
  (should (equal (custom-variable-default-value 'org-modules)
                 '(ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww))))

(provide 'custom-test)
