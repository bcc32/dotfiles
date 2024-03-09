;; -*- lexical-binding: t; -*-

(require 'ert)

;; TODO: Add tests for custom faces
;; Adapted from https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically
(defun custom-variable-default-value (var)
  (eval (car (get var 'standard-value))))

(ert-deftest test-eshell-visual-commands ()
  "`eshell-visual-commands' default value has not changed."
  :tags '(bcc32 custom)
  (require 'em-term)
  (should (equal (custom-variable-default-value 'eshell-visual-commands)
                 '("vi" "vim" "screen" "tmux" "top" "htop" "less"
                   "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm"))))

(ert-deftest test-eshell-visual-subcommands ()
  "`eshell-visual-subcommands' default value has not changed."
  :tags '(bcc32 custom)
  (require 'em-term)
  (should (equal (custom-variable-default-value 'eshell-visual-subcommands)
                 nil)))

(ert-deftest test-grep-files-aliases ()
  "`grep-files-aliases' default value has not changed."
  :tags '(bcc32 custom)
  (require 'grep)
  (should (equal (custom-variable-default-value 'grep-files-aliases)
                 '(("all" . "* .*")
                   ("el" . "*.el")
                   ("ch" . "*.[ch]")
                   ("c" . "*.c")
                   ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
                   ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
                   ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
                   ("h" . "*.h")
                   ("l" . "[Cc]hange[Ll]og*")
                   ("am" . "Makefile.am GNUmakefile *.mk")
                   ("m" . "[Mm]akefile*")
                   ("tex" . "*.tex")
                   ("texi" . "*.texi")
                   ("asm" . "*.[sS]")))))

(ert-deftest test-grep-find-ignored-directories ()
  "`grep-find-ignored-directories' default value has not changed."
  :tags '(bcc32 custom)
  (require 'grep)
  (should (equal (custom-variable-default-value 'grep-find-ignored-directories)
                 '("SCCS" "RCS" "CVS" "MCVS"
                   ".src" ".svn" ".git" ".hg" ".bzr"
                   "_MTN" "_darcs" "{arch}"))))

(ert-deftest test-magit-todos-exclude-globs ()
  "`magit-todos-exclude-globs' default value has not changed."
  :tags '(bcc32 custom)
  (require 'magit-todos)
  (should (equal (custom-variable-default-value 'magit-todos-exclude-globs)
                 '(".git/"))))

(ert-deftest test-org-modules ()
  "`org-modules' default value has not changed."
  :tags '(bcc32 custom)
  (require 'org)
  (should (equal (custom-variable-default-value 'org-modules)
                 '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info
                          ol-irc ol-mhe ol-rmail ol-eww))))

(ert-deftest test-safe-local-variable-values-is-sorted ()
  "Variables in `safe-local-variable-values' are in alphabetical order."
  :tags '(bcc32 custom)
  (let ((vars (mapcar #'car safe-local-variable-values)))
    (should (equal vars
                   (sort vars (lambda (a b) (string< (symbol-name a) (symbol-name b))))))))

(ert-deftest test-spacemacs-large-file-modes-list ()
  "`spacemacs-large-file-modes-list' default value has not changed."
  :tags '(bcc32 custom)
  (require 'org)
  (should (equal (custom-variable-default-value 'spacemacs-large-file-modes-list)
                 '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                                doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                                pdf-view-mode tags-table-mode fundamental-mode))))

(provide 'custom-test)
