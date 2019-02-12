;;; init.el --- bcc32's Spacemacs configuration  -*- lexical-binding: t -*-

;; https://github.com/bcc32/dotfiles

;;; Commentary:

;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;; Code:

(require 'seq)

(defun when-any-installed (executable-names &rest pkgs)
  "Only enable PKGS if any of EXECUTABLE-NAMES is installed in `exec-path'."
  (if (seq-some #'executable-find executable-names)
      pkgs))

(defun when-on-hostname (name &rest pkgs)
  "Only enable PKGS if current machine's hostname is NAME."
  (if (string= (system-name) name)
      pkgs))

(defun when-on-type (type &rest pkgs)
  "Only enable PKGS if `system-type' is `eql' to TYPE."
  (if (eql system-type type)
      pkgs))

;; TODO conditionally reimplement this in Ecaml
(defun bcc32/ansi-color-buffer ()
  "Render ANSI SGR color sequences in the current buffer."
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun bcc32//replace-buffer-contents (source)
  "Replace the current buffer contents with SOURCE.

Uses `replace-buffer-contents' if available."
  ;; FIXME replace-buffer-contents has a bug in 26.1, avoid
  (if (fboundp 'replace-buffer-contents)
      (replace-buffer-contents source)
    (let ((old-line (line-number-at-pos)))
      (delete-region (point-min) (point-max))
      (insert-buffer-substring source)
      ;; try to return to approximately where the point used to be
      (goto-char (point-min))
      (forward-line (1- old-line)))))

(defgroup bcc32 nil
  "Bcc32's customization options."
  :group 'emacs)

(defcustom bcc32/ocamlformat-program "ocamlformat"
  "Path to the ocamlformat program, used in `bcc32/ocamlformat-buffer'."
  :type '(choice file (const :tag "Disable ocamlformat" nil))
  :group 'bcc32)

(defun bcc32//ocamlformat-file-inplace (file)
  "Run `bcc32/ocamlformat-program' in-place on FILE."
  (when bcc32/ocamlformat-program
    (call-process bcc32/ocamlformat-program nil nil nil
                  "--inplace" file)))

(defcustom bcc32/ocp-indent-program "ocp-indent"
  "Path to the ocp-indent program, used in `bcc32/ocamlformat-buffer'."
  :type '(choice file (const :tag "Disable ocp-indent" nil))
  :group 'bcc32)

(defun bcc32//ocp-indent-file-inplace (file)
  "Run `bcc32/ocp-indent-program' in-place on FILE."
  (when bcc32/ocp-indent-program
    (call-process bcc32/ocp-indent-program nil nil nil
                  "--inplace" file)))

(defun bcc32/ocamlformat-buffer ()
  "Use ocamlformat and then ocp-indent to reformat the current buffer.

See also `bcc32/ocamlformat-program' and
`bcc32/ocp-indent-program'."
  (interactive "*")                     ;fail if buffer is read-only
  (let* ((filename (buffer-file-name))
         (extension (and filename (file-name-extension filename)
                         (concat "." (file-name-extension filename))))
         (temporary-file-directory default-directory)
         (tmpfile (make-temp-file "bcc32_ocamlformat" nil extension)))
    (write-region nil nil tmpfile nil :nomsg)
    (bcc32//ocamlformat-file-inplace tmpfile)
    (bcc32//ocp-indent-file-inplace tmpfile)
    (let ((tmpbuf (generate-new-buffer " bcc32/ocamlformat-buffer")))
      (with-current-buffer tmpbuf
        (insert-file-contents tmpfile))
      (bcc32//replace-buffer-contents tmpbuf)
      (kill-buffer tmpbuf))
    (delete-file tmpfile)))

(define-minor-mode bcc32/ocamlformat-on-save-mode
  "Minor mode to automatically run ocamlformat before saving OCaml code."
  :lighter "fmt"
  :global t
  :group 'bcc32
  (with-eval-after-load 'tuareg
    (add-hook 'before-save-hook 'bcc32//ocamlformat-on-save-hook)))

(defun bcc32//ocamlformat-on-save-hook ()
  "Run ocamlformat on the current buffer.

Suitable for use with `before-save-hook'."
  (when (and bcc32/ocamlformat-on-save-mode
             (derived-mode-p 'tuareg-mode))
    (bcc32/ocamlformat-buffer)))

(defun bcc32//org-cleanup ()
  "Run some cleanup on the current buffer, if it's an org buffer.

- Update dynamic blocks (e.g., clock reports)
- Update statistics cookies (e.g., [2/3])
- Align heading tags"
  "Update org mode statistics cookies and align all heading tags."
  (when (derived-mode-p 'org-mode)
    (org-update-all-dblocks)
    (org-update-statistics-cookies t)
    (org-align-all-tags)))

(defun bcc32//ledger-report-env-ledger-file-format-specifier ()
  "Return the value of the LEDGER_FILE environment variable."
  (getenv "LEDGER_FILE"))

(defun dotspacemacs/layers ()
  "Layer configuration: This function should only modify
configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   `(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; Emacs
     (finance :variables
              ledger-reports
              '(("reconcile" "ldg-reconcile %(account)")
                ("uncleared" "%(binary) reg -U '^Assets' '^Equity' '^Liabilities'")
                ("bal" "%(binary) bal")
                ("payee" "%(binary) reg @%(payee)")
                ("account" "%(binary) reg %(account)")
                ("validate" "%(binary) source %(env-ledger-file)")))
     ibuffer
     (ivy :variables
          ivy-use-virtual-buffers nil
          ivy-format-function 'ivy-format-function-arrow)
     ,@(when-on-type 'darwin 'osx)
     ;; Text editing
     auto-completion
     ,@(when-any-installed '("aspell" "ispell") 'spell-checking)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-use-original-bitmaps t
                      flycheck-idle-change-delay 1.0)
     ;; Programmer tools
     git
     nixos
     (shell :variables
            shell-default-shell 'multi-term
            multi-term-program "/bin/zsh")
     ;; Programming languages
     ;; Functional
     emacs-lisp
     ,@(when-any-installed '("ghc" "stack") 'haskell)
     ,@(when-any-installed '("ocamlc" "opam")
                           '(ocaml :variables tuareg-prettify-symbols-full t))
     ;; System
     c-c++
     ,@(when-any-installed '("go")
                           '(go :variables
                                gofmt-command "goimports"
                                go-tab-width 4))
     ,@(when-any-installed '("rustc" "cargo") 'rust)
     ;; Web
     html
     sql
     ;; Script
     perl5
     ,@(when-any-installed '("python") 'python)
     ,@(when-any-installed '("ruby") 'ruby)
     shell-scripts
     ;; Documents/markup
     csv
     ,@(when-any-installed '("xelatex" "pdflatex" "latexmk")
                           'bibtex
                           '(latex :variables
                                   latex-build-command "latexmk"
                                   TeX-engine 'xetex))
     markdown
     (org :variables
          ;;; Agenda

          ;; org-agenda-files are computed based on the contents of ~/org.
          ;;
          ;; ~/org should contain symlinks to org files or directories
          ;; containing org files.
          ;;
          ;; ~/org/default should be a symlink pointing to one of the entries in
          ;; ~/org that represents the default directory for notes.
          org-agenda-files
          (mapcar (lambda (file)
                    (file-truename
                     (concat (file-name-as-directory "~/org") file)))
                  (seq-difference (directory-files "~/org" nil nil :nosort)
                                  '("." ".." "default")))

          ;; Don't show scheduled items in the global todo list, because
          ;; presumably you don't want to think about them until the scheduled
          ;; time.
          org-agenda-tags-todo-honor-ignore-options t
          org-agenda-todo-ignore-scheduled 'future
          org-agenda-todo-ignore-time-comparison-use-seconds t

          ;;; Capture and refile

          org-capture-templates
          '(("t" "Todo" entry (file "")
             "* TODO %?\n%U\n%a"))
          org-default-notes-file "~/org/default/refile.org"
          org-directory "~/org/default"
          ;; Add file name to org refile target prompt.  This also allows an
          ;; entry to be refiled under a file's toplevel.
          ;; https://emacs.stackexchange.com/questions/13353/how-to-use-org-refile-to-move-a-headline-to-a-file-as-a-toplevel-headline
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-targets '((nil :maxlevel . 3)
                               (org-agenda-files :maxlevel . 3))

          ;;; Org export

          org-export-backends '(ascii html icalendar latex org texinfo)
          org-html-htmlize-output-type 'css
          org-html-htmlize-font-prefix "org-"

          ;;; Editing

          org-insert-heading-respect-content t
          org-startup-indented t

          ;;; Org TODO configuration

          ;; Log state change notes and time stamps into LOGBOOK drawer.
          org-log-into-drawer t
          org-stuck-projects '("+LEVEL=2/!" ("TODO" "MAYBE" "INPROGRESS") nil "")
          org-todo-keywords '((sequence "TODO(t)"
                                        "MAYBE(m/!)"
                                        "INPROGRESS(p!)"
                                        "STALLED(a)"
                                        "BLOCKED(k@/!)"
                                        "INREVIEW(r!)"
                                        "|"
                                        "DONE(d)"
                                        "DELEGATED(g@)"
                                        "DEFERRED(e)"
                                        "NOTDONE(n)")
                              (type "BUG(b/!)"
                                    "CLEANUP(l/!)"
                                    "|"
                                    "FIXED(x)"
                                    "WONTFIX(w@)")
                              (type "ENHANCEMENT(h/!)"
                                    "FEATURE(f/!)"
                                    "|"
                                    "RELEASED(s)"
                                    "CANCELED(c)")))
     yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(base16-theme
     solarized-theme
     tao-theme
     zenburn-theme)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(helm
                                    helm-core
                                    helm-make
                                    smex
                                    treemacs
                                    treemacs-evil
                                    treemacs-projectile)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         base16-material
                         zenburn
                         leuven
                         tao-yin
                         tao-yang
                         solarized-light
                         solarized-dark
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator arrow-fade :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka"
                               :size 15
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols nil

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers
   '(:relative t
     :enabled-for-modes prog-mode
                        text-mode
                        tuareg-mode
     :disabled-for-modes org-mode)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%U@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq custom-file "~/.spacemacs.d/custom.el")
  (load custom-file))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (global-set-key (kbd "C-x C-c")
                  (lambda () (interactive) (error "Do not use C-x C-c")))

  ;; toggles
  (spacemacs/toggle-display-time-on)
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (spacemacs/toggle-indent-guide-globally-on)
  (spacemacs/toggle-mode-line-battery-on)
  (spacemacs/toggle-mode-line-org-clock-on)

  ;; theme customization
  (setq solarized-distinct-doc-face t
        solarized-use-more-italic t)

  (spacemacs/set-leader-keys
    "Ca" 'bcc32/ansi-color-buffer)

  (with-eval-after-load 'tuareg
    (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
      "v" 'merlin-enclosing-expand
      "f" 'bcc32/ocamlformat-buffer
      "F" 'bcc32/ocamlformat-on-save-mode))

  (with-eval-after-load 'cperl-mode
    (setq cperl-close-paren-offset -2)
    (setq cperl-indent-parens-as-block t))

  (with-eval-after-load 'org
    (spacemacs/set-leader-keys
      "aog" 'counsel-org-goto-all)
    (add-hook 'before-save-hook 'bcc32//org-cleanup))

  (with-eval-after-load 'ledger-mode
    (defvar ledger-default-date-format)
    (defvar ledger-iso-date-format)
    (defvar ledger-report-format-specifiers)
    (setq ledger-default-date-format ledger-iso-date-format)
    (add-to-list 'ledger-report-format-specifiers
                 '("env-ledger-file" . bcc32//ledger-report-env-ledger-file-format-specifier)))

  (setq comment-style 'multi-line)
  (setq company-idle-delay 1.0)
  (setq-default fill-column 80)
  (setq powerline-default-separator 'arrow)
  (setq-default sentence-end-double-space t))

;;; init.el ends here
