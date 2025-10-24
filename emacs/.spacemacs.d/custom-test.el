;; -*- lexical-binding: t; -*-

(require 'ert)

(defun custom-face-default-spec (face)
  (get face 'face-defface-spec))

(ert-deftest test-grep-files-aliases ()
  "`grep-files-aliases' default value has not changed."
  :tags '(bcc32 custom)
  (require 'grep)
  (should (equal (custom--standard-value 'grep-files-aliases)
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
  (should (equal (custom--standard-value 'grep-find-ignored-directories)
                 '("SCCS" "RCS" "CVS" "MCVS"
                   ".src" ".svn" ".git" ".hg" ".bzr"
                   "_MTN" "_darcs" "{arch}"))))

(ert-deftest test-magit-todos-exclude-globs ()
  "`magit-todos-exclude-globs' default value has not changed."
  :tags '(bcc32 custom)
  (require 'magit-todos)
  (should (equal (custom--standard-value 'magit-todos-exclude-globs)
                 '(".git/"))))

(ert-deftest test-org-modules ()
  "`org-modules' default value has not changed."
  :tags '(bcc32 custom)
  (require 'org)
  (should (equal (custom--standard-value 'org-modules)
                 '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info
                          ol-irc ol-mhe ol-rmail ol-eww))))

(ert-deftest test-org-link ()
  "`org-link' default face spec has not changed."
  :tags '(bcc32 custom face)
  (require 'org-faces)
  (should (equal (custom-face-default-spec 'org-link)
                 '((t :inherit link)))))

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
  (should (equal (custom--standard-value 'spacemacs-large-file-modes-list)
                 '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                                doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                                pdf-view-mode tags-table-mode fundamental-mode))))

(ert-deftest test-tuareg-font-lock-attribute-face ()
  "`tuareg-font-lock-attribute-face' default face spec has not changed."
  :tags '(bcc32 custom face)
  (require 'tuareg)
  (should (equal (custom-face-default-spec 'tuareg-font-lock-attribute-face)
                 '((t :inherit font-lock-preprocessor-face)))))

(ert-deftest test-tuareg-font-lock-extension-node-face ()
  "`tuareg-font-lock-extension-node-face' default face spec has not changed."
  :tags '(bcc32 custom face)
  (require 'tuareg)
  (should (equal (custom-face-default-spec 'tuareg-font-lock-extension-node-face)
                 '((default :inherit tuareg-font-lock-infix-extension-node-face)
                   (((background dark)) :foreground "LightSteelBlue")
                   (t :background "gray92")))))

(ert-deftest test-tuareg-font-lock-infix-extension-node-face ()
  "`tuareg-font-lock-infix-extension-node-face' default face spec has not changed."
  :tags '(bcc32 custom face)
  (require 'tuareg)
  (should (equal (custom-face-default-spec 'tuareg-font-lock-infix-extension-node-face)
                 '((t :inherit font-lock-preprocessor-face)))))

(ert-deftest test-vc-handled-backends ()
  "`vc-handled-backends' default value has not changed."
  :tags '(bcc32 custom)
  (should (equal (custom--standard-value 'vc-handled-backends)
                 '(RCS CVS SVN SCCS SRC Bzr Git Hg))))

(provide 'custom-test)
