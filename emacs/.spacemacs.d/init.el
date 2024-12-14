;;; init.el --- bcc32's Spacemacs configuration  -*- lexical-binding: t -*-

;; https://github.com/bcc32/dotfiles

;;; Commentary:

;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'seq)
(eval-when-compile (require 'subr-x))

(declare-function ansi-color-apply-on-region "ansi-color")
(defun bcc32/ansi-color-region-or-buffer ()
  "Render ANSI SGR color sequences in the current region if it is active.

Otherwise, render sequences in the current buffer."
  (interactive)
  (require 'ansi-color)
  (with-silent-modifications
    (save-restriction
      (when (use-region-p) (narrow-to-region (region-beginning) (region-end)))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defun bcc32/browse-url-on-ssh-client-if-exists (url &rest args)
  "Browse URL on the selected frame's $SSH_CONNECTION, if it exists.

Fall back to `browse-url-default-browser' if SSH_CONNECTION is
unset in the selected frame, passing ARGS."
  (if-let ((ssh-connection (getenv "SSH_CONNECTION" (selected-frame))))
      (with-current-buffer (get-buffer-create "*browse-on-ssh-client*")
        (make-local-variable 'process-environment)
        (push (concat "SSH_CONNECTION=" ssh-connection) process-environment)
        (if (equal 0 (call-process (expand-file-name "~/bin/,browse-on-ssh-client")
                                   nil t nil url))
            (kill-buffer)
          (display-buffer (current-buffer))
          (error "Failed to browse URL on SSH client")))
    (apply #'browse-url-default-browser url args)))

(defun bcc32/browse-url-on-wsl (url &rest args)
  "Browse URL on the host Windows machine, if running in WSL.

Fall back to `browse-url-default-browser' if cmd.exe is not
found, passing ARGS."
  (if-let ((cmd (executable-find "cmd.exe")))
      (call-process cmd nil nil nil
                    "/c"
                    (format "start /wait /b %s" url))
    (apply #'browse-url-default-browser url args)))

(defun bcc32/kill-ring-save-refill-for-web (beg end)
  "Copy the region BEG to END to the kill ring, refilling the text for web use.

When pasting into web forms, newlines separate paragraphs, unlike
in Emacs where paragraphs are delimited by empty lines.  This
command fills the copied text with no newlines within paragraphs."
  (interactive "r")
  (let ((filter-buffer-substring-function
         (lambda (beg end delete)
           (let ((contents (if delete
                               (delete-and-extract-region beg end)
                             (buffer-substring beg end))))
             (with-temp-buffer
               (insert contents)
               (setq fill-column most-positive-fixnum)
               (indent-region (point-min) (point-max))
               (fill-region (point-min) (point-max))
               (buffer-string))))))
    (kill-ring-save beg end)))

(defun bcc32/projectile-ignored-project-function (project-root)
  "Return t if a project rooted at PROJECT-ROOT should be ignored by projectile."
  (or (string-prefix-p "/nix/store/" project-root)
      (string-match-p (regexp-quote "/.cargo/registry/") project-root)))

(defun bcc32//set-fill-column-in-text-mode-hook ()
  "Set `fill-column' to 70 characters in derived modes of `text-mode'."
  (unless (when-let ((hash (bound-and-true-p editorconfig-properties-hash)))
            (gethash 'max_line_length hash))
    (setq-local fill-column 70)))

(declare-function sp-local-pair "smartparens")
(declare-function sp-update-local-pairs "smartparens")
(defun bcc32--work-around-smartparens-1036 ()
  "Work around Fuco1/smartparens#1036.

`smartparens-strict-mode' has the wrong behavior for
`eval-expression'.

Intended to be added to `eval-expression-minibuffer-setup-hook'."
  (sp-local-pair 'eval-expression-minibuffer "'" nil :actions nil)
  (sp-local-pair 'eval-expression-minibuffer "`" nil :actions nil)
  (sp-update-local-pairs 'eval-expression-minibuffer))

(defvar dotspacemacs-directory)
(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   `(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; Checkers
     spell-checking
     syntax-checking

     ;; Completion
     ;; (auto-completion :variables
     ;;                  auto-completion-idle-delay nil)
     ;; ivy
     compleseus

     ;; E-mail
     ,(when (executable-find "mu") 'mu4e)

     ;; Emacs
     ;; helpful
     ibuffer
     (org :variables
          org-enable-appear-support t
          org-enable-github-support t
          org-enable-modern-support t
          org-enable-notifications t
          org-project-capture-projects-file "TODO.org"
          org-start-notification-daemon-on-startup nil
          org-want-todo-bindings t)
     pdf
     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t)

     ;; Programming and markup languages
     ;; lsp
     autohotkey
     ;; c-c++
     csv
     ;; common-lisp
     emacs-lisp
     ;; (go :variables
     ;;     go-format-before-save t
     ;;     go-tab-width 4)
     ;; haskell
     ;; html
     ;; (javascript :variables
     ;;             javascript-backend 'tide
     ;;             javascript-fmt-on-save t
     ;;             javascript-fmt-tool 'prettier)
     json
     ;; bibtex
     ;; latex
     markdown
     (ocaml :variables
            ocaml-format-on-save t)
     ;; perl5
     ;; (python :variables
     ;;         python-format-on-save t
     ;;         python-formatter 'black
     ;;         python-sort-imports-on-save t)
     ;; ruby
     (rust :variables
           cargo-process-reload-on-modify t
           rust-backend nil)
     (scheme :variables
             scheme-implementations '(chicken guile))
     (shell-scripts :variables
                    shell-scripts-backend nil
                    shell-scripts-format-on-save t)
     sql
     ;; (typescript :variables
     ;;             typescript-backend 'tide
     ;;             typescript-fmt-on-save t
     ;;             typescript-fmt-tool 'prettier)
     ;; yaml

     ;; Operating systems
     (nixos :variables
            nixos-format-on-save t)
     osx

     ;; Source control
     (git :variables
          git-enable-magit-todos-plugin t)
     (version-control :variables
                      version-control-diff-tool nil)

     ;; Spacemacs distribution layers
     spacemacs-completion
     spacemacs-editing
     spacemacs-editing-visual
     spacemacs-evil
     spacemacs-language
     spacemacs-misc
     spacemacs-modeline
     spacemacs-navigation
     spacemacs-org
     spacemacs-project
     spacemacs-visual

     ;; Tools
     ;; command-log
     ;; fasd
     finance
     (shell :variables
            shell-default-shell 'shell
            shell-enable-smart-eshell t)
     (transmission :variables
                   transmission-auto-refresh-all t)

     ;; Web services
     ;; spotify

     ;; User layers
     (bcc32 :variables
            bcc32-enable-auto-revert-debugging-at-startup nil
            bcc32-enable-explain-pause-at-startup nil)
     bcc32-ledger
     bcc32-org
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(base16-theme
     solarized-theme
     tao-theme
     zenburn-theme

     (bcc32-private
      :location (recipe :fetcher file
                        :path "~/src/dotfiles-private/Emacs")))

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     multi-vterm
     pdf-view-restore ;this package litters .pdf-view-restore files all over my disk
     popwin           ;annoying behavior with *Help* buffers.
     vterm
     )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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
   dotspacemacs-editing-style
   '(hybrid :variables
            vim-style-enable-undo-region t
            vim-style-remap-Y-to-y$ t)

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable t

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         modus-vivendi
                         base16-material
                         leuven
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme 'vanilla

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font `("Iosevka"
                               :size ,(if (eq system-type 'darwin) 14 10.5)
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

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
   dotspacemacs-max-rollback-slots most-positive-fixnum

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
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

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
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

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

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers
   '(:visual t :disabled-for-modes org-mode)

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
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

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%U@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(declare-function spacemacs/load-spacemacs-env "core-env")
(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session.  By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (when (eq system-type 'darwin)
    (spacemacs/load-spacemacs-env)))

(define-advice package--save-selected-packages (:override (&rest _) dont-save-to-custom-file -100)
  "Don't save `package-selected-packages' to `custom-file'."
  nil)

(define-advice recentf-cleanup (:after (&rest _) cleanup-file-name-history)
  "Clean up `file-name-history' after cleaning up recentf lists."
  (setq file-name-history
        (cl-delete-if (lambda (file)
                        (or (file-remote-p file) (not (file-exists-p file))))
                      file-name-history)))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq source-directory "~/src/emacs/trunk/")
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file)
  (add-to-list 'load-path dotspacemacs-directory)
  (let ((default-directory "~/.nix-profile/share/emacs/site-lisp"))
    (when (file-directory-p default-directory)
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path)
      ;; Emulate behavior of `package-directory-list'.
      (add-to-list 'package-directory-list (expand-file-name "elpa")))))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration.  You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defvar spaceline-org-clock-p)
(defvar spaceline-version-control-p)
(defvar spacemacs-tuareg-mode-map)

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (declare-function dired-omit-mode "dired-x")
  (declare-function merlin-enclosing-expand "merlin")
  (declare-function ocamlformat "ocamlformat")
  (declare-function spacemacs/enable-flycheck "~/.emacs.d/layers/+checkers/syntax-checking/funcs.el")
  (declare-function spacemacs/save-buffers-kill-emacs "~/.emacs.d/layers/+spacemacs/spacemacs-defaults/funcs.el")

  (add-hook 'eval-expression-minibuffer-setup-hook #'bcc32--work-around-smartparens-1036)

  (put 'list-timers 'disabled nil)

  ;; Enable completions for `Font Family’ field in `M-x customize-face RET’
  ;; https://emacsnotes.wordpress.com/2024/11/03/enable-completions-for-font-family-field-in-m-x-customize-face-ret/
  (advice-add 'widget-complete :before
              (defun widget-complete--before (&rest orig-args)
                (pcase-let* ((`nil orig-args))
                  (when (equal (widget-get (widget-at) :tag) "Font Family")
                    (widget-put (widget-at) :completions (font-family-list))))))

  ;; Replace `spacemacs/kill-emacs' and `spacemacs/prompt-kill-emacs' with
  ;; `spacemacs/save-buffers-kill-emacs'.  The former functions do not properly
  ;; run `kill-emacs-query-functions' and so will kill buffers that have
  ;; processes running.
  (bind-keys ([remap spacemacs/kill-emacs] . spacemacs/save-buffers-kill-emacs)
             ([remap spacemacs/prompt-kill-emacs] . spacemacs/save-buffers-kill-emacs))

  (bind-key [remap zap-to-char] #'zap-up-to-char)

  ;; Superword mode, for evil
  (with-eval-after-load 'evil
    (declare-function forward-evil-symbol "evil-common")
    (defalias 'forward-evil-word #'forward-evil-symbol))

  ;; mode line segments
  (setq spaceline-org-clock-p t)
  (setq spaceline-version-control-p nil)

  (spacemacs/enable-flycheck 'emacs-lisp-mode)

  (with-eval-after-load 'ivy
    (defvar ivy-format-functions-alist)
    (declare-function ivy-format-function-line "ivy")
    (setf (alist-get t ivy-format-functions-alist)
          #'ivy-format-function-line))

  (if (version-list-<= (package-desc-version (cadr (assq 'org-contrib package-alist)))
                       '(0 6))
      (with-eval-after-load 'org-expiry
        ;; Loading this package (which can happen due to autoloads during completion over symbols)
        ;; unfortunately immediately activates the advice.
        ;;
        ;; Fixed upstream in https://git.sr.ht/~bzg/org-contrib/commit/82d33ade0925d1a170d6d9e9f71051655a2a2e6e
        (org-expiry-deinsinuate))
    (warn "org-expiry workaround is no longer necessary."))

  (bind-keys :map spacemacs-tuareg-mode-map
             ("f" . ocamlformat)
             ("v" . merlin-enclosing-expand))

  (add-hook 'text-mode-hook #'bcc32//set-fill-column-in-text-mode-hook)
  (add-hook 'text-mode-hook #'turn-on-auto-fill)

  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (require 'bcc32-abbrev)
  (declare-function bcc32-ocaml-abbrevs "bcc32-abbrev" ())
  (bcc32-ocaml-abbrevs)

  (with-eval-after-load 'savehist
    (defvar savehist-additional-variables)
    (add-to-list 'savehist-additional-variables 'log-edit-comment-ring))

  (use-package mu4e
    :defer t
    :config
    ;; TODO: Refactor if mu4e-marks becomes more easily configurable.
    ;; https://github.com/djcb/mu/issues/1136#issuecomment-1066303788
    (setf (plist-get (alist-get 'trash mu4e-marks) :action)
          (lambda (docid msg target)
            ;; Here's the main difference to the regular trash mark,
            ;; no +T before -N so the message is not marked as
            ;; IMAP-deleted:
            (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
    (setf (plist-get (alist-get 'refile mu4e-marks) :action)
          (lambda (docid msg target)
            ;; mark messages read when archiving
            (mu4e--server-move docid (mu4e--mark-check-target target) "+S-N"))))

  ;; TODO: Maybe which-func should do this *always*, upstream?  (Follow up with
  ;; a comment at https://github.com/minad/org-modern/pull/235 if I report it
  ;; upstream.)
  (setq-default which-func-cleanup-function #'substring-no-properties)

  (lossage-size 3000)

  ;; Make sure my customizations take precedence over settings that Spacemacs
  ;; `setq's, even after running `dotspacemacs/sync-configuration-layers'.
  ;;
  ;; This would not be necessary if my customizations were in this file instead
  ;; of a separate file.
  (load custom-file))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

;;; init.el ends here
