;;; init.el --- bcc32's Spacemacs configuration  -*- lexical-binding: t -*-

;; https://github.com/bcc32/dotfiles

;;; Commentary:

;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;; Code:

(require 'browse-url)
(require 'cl-lib)

(defun bcc32/browse-url-on-ssh-client-if-exists (url &rest args)
  "Browse URL on the selected frame's $SSH_CONNECTION, if it exists.

Fall back to `browse-url-default-browser' if SSH_CONNECTION is
unset in the selected frame, passing ARGS."
  (if-let* ((ssh-connection (getenv "SSH_CONNECTION" (selected-frame))))
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
  (if-let* ((cmd (executable-find "cmd.exe")))
      (call-process cmd nil nil nil
                    "/c"
                    (format "start /wait /b %s" url))
    (apply #'browse-url-default-browser url args)))

(defun bcc32/projectile-ignored-project-function (project-root)
  "Return t if a project rooted at PROJECT-ROOT should be ignored by projectile."
  (or (file-remote-p project-root)
      (string-prefix-p "/nix/store/" project-root)
      (string-search "/.cargo/registry/" project-root)))

(defun bcc32//set-fill-column-in-text-mode-hook ()
  "Set `fill-column' to 70 characters in derived modes of `text-mode'."
  (unless (and-let* ((hash (bound-and-true-p editorconfig-properties-hash)))
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

;; https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/
(defun er-keyboard-quit ()
  "Smarter version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
  (interactive)
  (if (active-minibuffer-window)
      (if (minibufferp)
          (abort-minibuffers)
        (abort-recursive-edit))
    (keyboard-quit)))

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
     eglot
     ;; lsp
     autohotkey
     ;; c-c++
     csv
     common-lisp
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
     lua
     markdown
     (ocaml :variables
            ocaml-format-on-save t)
     ;; perl5
     (python :variables
             python-format-on-save t
             python-formatter 'ruff
             python-sort-imports-on-save t)
     ;; ruby
     (rust :variables
           rust-backend nil)
     (scheme :variables
             scheme-implementations '(chez chicken guile))
     (shell-scripts :variables
                    shell-scripts-backend nil
                    shell-scripts-format-on-save t)
     (sql :variables
          sql-capitalize-keywords t)
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
            shell-default-shell 'eat
            shell-enable-smart-eshell t
            shell-enable-vterm-support nil)
     (transmission :variables
                   transmission-auto-refresh-all t)

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
   `(solarized-theme
     tao-theme
     zenburn-theme

     olivetti

     (bcc32-private
      :location (recipe :fetcher file
                        :path "~/src/dotfiles-private/Emacs"))

     (wl :location (recipe :fetcher github
                           :repo "bcc32/watch-later"))
     ,@(when (executable-find "journalctl") '(journalctl-mode)))

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     flycheck-elsa
     overseer                           ;loads itself on emacs-lisp-mode-hook
                                        ;even if it would not enable itself.
     info+
     pdf-view-restore ;this package litters .pdf-view-restore files all over my disk
     popwin           ;annoying behavior with *Help* buffers.
     )

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
   ;; This has no effect in terminal or if "nerd-icons" package or the font
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
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(
                         modus-vivendi
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
   dotspacemacs-default-font
   '(("Cascadia Code" :size 16.0)
     ("Iosevka"       :size 16.0))

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'all-the-icons

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
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "M-<return>" "C-M-m")

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

   ;; Make consecutive tab key presses after commands such as
   ;; `spacemacs/alternate-buffer' (SPC TAB) cycle through previous
   ;; buffers/windows/etc. Please see the option's docstring for more information.
   ;; Set the option to t in order to enable cycling for all current and
   ;; future cycling commands. Alternatively, choose a subset of the currently
   ;; supported commands: '(alternate-buffer alternate-window). (default nil)
   dotspacemacs-enable-cycling nil

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
   dotspacemacs-line-numbers '(:visual t :disabled-for-modes prog-mode text-mode)

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
   ;; tool of the list. Supported tools are `rg', `ag', `ack' and `grep'.
   ;; (default '("rg" "ag" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-redo', `undo-fu' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system from or to undo-tree. (default `undo-redo')
   dotspacemacs-undo-system 'undo-redo

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
   dotspacemacs-use-clean-aindent-mode nil

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
   dotspacemacs-pretty-docs nil

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

;; TODO: This should probably just be part of Spacemacs, if it's going to take
;; over installed packages via spacemacs-sync-packages anyway.
(define-advice package--save-selected-packages (:override (&rest _) dont-save-to-custom-file -100)
  "Don't save `package-selected-packages' customization."
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
  )

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
  (bind-key [remap keyboard-quit] #'er-keyboard-quit)

  (with-eval-after-load 'embark
    (bind-key "g" (lambda (dir)
                    (interactive "Ddirectory: ")
                    (magit-status dir))
              embark-file-map))

  (with-eval-after-load 'eshell
    (add-to-list 'eshell-visual-commands "ncdu")
    (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))

  ;; Superword mode, for evil
  (with-eval-after-load 'evil
    (declare-function forward-evil-symbol "evil-common")
    (declare-function evil-set-initial-state "evil-core")
    (defalias 'forward-evil-word #'forward-evil-symbol)
    (evil-set-initial-state 'shell-command-mode 'normal))

  (spacemacs/enable-flycheck 'emacs-lisp-mode)

  (with-eval-after-load 'ivy
    (defvar ivy-format-functions-alist)
    (declare-function ivy-format-function-line "ivy")
    (setf (alist-get t ivy-format-functions-alist)
          #'ivy-format-function-line))

  (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
    "f" #'ocamlformat
    "v" #'merlin-enclosing-expand)

  (add-hook 'text-mode-hook #'bcc32//set-fill-column-in-text-mode-hook)
  (add-hook 'text-mode-hook #'turn-on-auto-fill)

  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (with-eval-after-load 'dired
    (dired-async-mode))

  (with-eval-after-load 'savehist
    (defvar savehist-additional-variables)
    (add-to-list 'savehist-additional-variables 'log-edit-comment-ring))

  (with-eval-after-load 'undo-fu-session
    (add-to-list 'undo-fu-session-incompatible-files
                 (lambda (file)
                   (file-in-directory-p file "~/todo/"))))

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
            (mu4e--server-move docid (mu4e--mark-check-target target) "+S-N")))
    (add-hook 'gnus-article-mode-hook #'spacemacs/disable-hl-line-mode)
    (add-hook 'gnus-article-mode-hook (lambda () (evil-quickscope-mode -1))))

  (lossage-size 3000))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

;;; init.el ends here
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(advent-of-code-cookie-jar "~/src/advent-of-code/cookies.txt")
   '(advent-of-code-email "me@bcc32.com")
   '(after-save-hook '(executable-make-buffer-file-executable-if-script-p))
   '(async-shell-command-buffer 'new-buffer)
   '(async-shell-command-display-buffer nil)
   '(breadcrumb-mode t)
   '(browse-url-browser-function 'bcc32/browse-url-on-ssh-client-if-exists)
   '(calc-context-sensitive-enter t)
   '(calc-settings-file
     (expand-file-name "calc-settings.el" dotspacemacs-directory))
   '(calendar-chinese-all-holidays-flag t)
   '(calendar-latitude 40.65)
   '(calendar-location-name "New York City")
   '(calendar-longitude -73.95)
   '(calendar-mark-holidays-flag t)
   '(calendar-week-start-day 1)
   '(column-enforce-column nil)
   '(completions-detailed t)
   '(corfu-auto-delay 0.5)
   '(counsel-org-goto-all-outline-path-prefix 'buffer-name)
   '(diary-file "~/todo/diary")
   '(diary-nongregorian-marking-hook '(diary-chinese-mark-entries))
   '(dired-async-skip-fast t)
   '(dired-auto-revert-buffer t)
   '(dired-omit-verbose nil)
   '(dired-recursive-copies 'always)
   '(dired-vc-rename-file t)
   '(display-time-mode t)
   '(eat-kill-buffer-on-exit t)
   '(echo-keystrokes-help nil)
   '(editorconfig-mode-lighter "")
   '(eglot-autoshutdown t)
   '(eglot-confirm-server-edits nil)
   '(enable-recursive-minibuffers t)
   '(enable-remote-dir-locals t)
   '(epg-pinentry-mode 'loopback)
   '(evil-escape-excluded-major-modes '(term-mode))
   '(evil-ex-visual-char-range t)
   '(evil-kill-on-visual-paste nil)
   '(evil-shift-round nil)
   '(evil-symbol-word-search t)
   '(evil-want-Y-yank-to-eol t)
   '(evil-want-fine-undo t)
   '(explicit-shell-file-name "zsh")
   '(fancy-battery-mode t)
   '(flycheck-emacs-lisp-initialize-packages t nil nil "Set so that ~/.spacemacs.d/init.el is checked properly")
   '(flycheck-emacs-lisp-load-path '("./"))
   '(flycheck-ledger-explicit t)
   '(flycheck-ledger-pedantic t)
   '(garbage-collection-messages t)
   '(geiser-default-implementation 'chicken)
   '(global-column-enforce-mode t)
   '(gofmt-command "goimports")
   '(grep-files-aliases
     '(("ml" . "*.ml *.mli *.mll *.mlt *.mly *.mdx") ("all" . "* .[!.]* ..?*")
       ("el" . "*.el") ("ch" . "*.[ch]") ("c" . "*.c")
       ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
       ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
       ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++") ("h" . "*.h")
       ("l" . "[Cc]hange[Ll]og*") ("am" . "Makefile.am GNUmakefile *.mk")
       ("m" . "[Mm]akefile*") ("tex" . "*.tex") ("texi" . "*.texi")
       ("asm" . "*.[sS]")))
   '(grep-find-ignored-directories
     '("_build" "_opam" "SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr"
       "_MTN" "_darcs" "{arch}"))
   '(helm-make-nproc 0)
   '(help-enable-variable-value-editing t)
   '(holiday-bahai-holidays nil)
   '(imenu-max-item-length 300)
   '(indicate-buffer-boundaries 'left)
   '(initial-buffer-choice t)
   '(ivy-magic-tilde nil)
   '(ivy-use-virtual-buffers nil)
   '(js-indent-level 2)
   '(ledger-accounts-exclude-function 'bcc32-ledger-accounts-exclude-function)
   '(ledger-copy-transaction-insert-blank-line-after t)
   '(ledger-default-date-format ledger-iso-date-format)
   '(ledger-reconcile-buffer-line-format "%(date)s %-20(code)s %-30(payee)s %15(amount)s\12")
   '(ledger-reconcile-buffer-payee-max-chars 30)
   '(ledger-reconcile-default-commodity "USD")
   '(ledger-reconcile-insert-effective-date 'bcc32-ledger-should-insert-effective-date)
   '(ledger-report-links-beginning-of-xact nil)
   '(ledger-report-use-header-line t)
   '(ledger-reports
     '(("debts" "%(binary) -f %(ledger-file) register --by-payee 'Personal Debts'")
       ("deposits" "ledger reg '^Assets:Deposits' --by-payee")
       ("net-worth"
        "%(binary) -f %(ledger-file) bal '^Assets' '^Liabilities' -X USD")
       ("reconcile"
        "%(binary) -f %(ledger-file) reg --current --effective --sort '(X ? 0 : 1), d' -d \"!X || d>=[90 days ago]\" \"/^%(account)$/\" | tac")
       ("reimbursement"
        "%(binary) -f %(ledger-file) --group-by payee reg -U '^Assets:Reimbursable Expenses'")
       ("uncleared"
        "%(binary) -f %(ledger-file) reg --current -U --group-by account '^Assets' '^Equity' '^Liabilities'")
       ("bal" "%(binary) -f %(ledger-file) bal")
       ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
       ("account" "%(binary) -f %(ledger-file) reg %(account)")))
   '(lsp-enable-suggest-server-download nil)
   '(lsp-inlay-hint-enable t)
   '(lsp-rust-analyzer-completion-add-call-parenthesis nil)
   '(lsp-solargraph-use-bundler t)
   '(magit-clone-default-directory "~/src/")
   '(magit-diff-refine-hunk t)
   '(magit-published-branches '("origin/main" "origin/master"))
   '(magit-section-initial-visibility-alist '((untracked . show) (stashes . hide)))
   '(magit-todos-exclude-globs '("flycheck_*" "*.org_archive" ".git/"))
   '(magit-todos-update 120)
   '(merlin-command "ocamlmerlin")
   '(merlin-eldoc-doc nil)
   '(merlin-eldoc-type nil)
   '(midnight-mode t)
   '(minibuffer-depth-indicate-mode t)
   '(mm-discouraged-alternatives '("text/html" "text/richtext" "image/.*"))
   '(mu4e-attachment-dir "~/Downloads/")
   '(mu4e-bookmarks
     '((:name "Inbox" :query "maildir:/INBOX" :key 105)
       (:name "Unread messages" :query
              "flag:unread AND NOT flag:trashed AND NOT maildir:/Spam AND NOT maildir:/Newsletters*"
              :key 117)
       (:name "TODO" :query "maildir:/TODO*" :key 84)
       (:name "Today's messages" :query
              "date:today..now AND NOT maildir:/Trash AND NOT maildir:/Spam" :key
              116)
       (:name "Last 7 days" :query
              "date:7d..now AND NOT maildir:/Trash AND NOT maildir:/Spam"
              :hide-unread t :key 119)
       (:name "Messages with images" :query "mime:image/*" :key 112)))
   '(mu4e-compose-complete-only-personal t)
   '(mu4e-drafts-folder "/Drafts")
   '(mu4e-get-mail-command "~/bin/,offlineimap")
   '(mu4e-headers-date-format "%F")
   '(mu4e-headers-fields
     '((:human-date . 12) (:flags . 6) (:maildir . 20) (:from-or-to . 22) (:subject)))
   '(mu4e-headers-time-format "%T")
   '(mu4e-hide-index-messages t)
   '(mu4e-maildir-shortcuts
     '((:maildir "/INBOX" :key 105) (:maildir "/TODO.Input into ledger" :key 108)
       (:maildir "/Newsletters.to read" :key 110)))
   '(mu4e-refile-folder "/Archive")
   '(mu4e-search-skip-duplicates nil)
   '(mu4e-sent-folder "/Sent")
   '(mu4e-trash-folder "/Trash")
   '(mu4e-update-interval 180)
   '(orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefixes))
   '(org-M-RET-may-split-line '((default)))
   '(org-agenda-dim-blocked-tasks t)
   '(org-agenda-files (mapcar #'file-truename '("~/todo/main/" "~/todo/low/")) nil nil "Must not contain symlinks or else auto-revert breaks, bug #65904")
   '(org-agenda-include-diary t)
   '(org-agenda-mouse-1-follows-link t)
   '(org-agenda-prefer-last-repeat t)
   '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
   '(org-agenda-skip-scheduled-if-done t)
   '(org-agenda-span 'day)
   '(org-agenda-sticky t)
   '(org-agenda-tags-todo-honor-ignore-options t)
   '(org-agenda-todo-ignore-scheduled 'future)
   '(org-agenda-todo-ignore-time-comparison-use-seconds t)
   '(org-agenda-window-setup 'other-window)
   '(org-appear-autolinks nil)
   '(org-capture-templates
     '(("g" "Groceries" entry (file+olp "~/todo/main/shopping-list.org" "Groceries")
        "* TODO %? %^g" :hook evil-insert-state)
       ("r" "Reading list entry" entry (file "~/todo/low/reading-list.org")
        "* TODO [[%c][%^{title}]] %^g\12%U")
       ("t" "Todo" entry (file "") "* TODO %?\12%U\12%a" :hook evil-insert-state)))
   '(org-clock-history-length 35)
   '(org-clock-persist t)
   '(org-default-notes-file "~/todo/main/refile.org")
   '(org-enforce-todo-checkbox-dependencies t)
   '(org-enforce-todo-dependencies t)
   '(org-export-backends '(ascii gfm html icalendar latex md org texinfo))
   '(org-extend-today-until 2)
   '(org-fontify-whole-heading-line t)
   '(org-habit-graph-column 80)
   '(org-habit-show-habits-only-for-today nil)
   '(org-html-htmlize-output-type 'css)
   '(org-insert-heading-respect-content t)
   '(org-log-into-drawer t)
   '(org-log-redeadline 'note)
   '(org-log-reschedule 'time)
   '(org-modern-hide-stars nil)
   '(org-modern-priority nil)
   '(org-modern-tag nil)
   '(org-modern-todo nil)
   '(org-modules '(org-habit ol-info org-tempo org-checklist ol-man))
   '(org-outline-path-complete-in-steps nil)
   '(org-pretty-entities t)
   '(org-priority-default 67)
   '(org-refile-target-verify-function 'bcc32-org-refile-verify-target)
   '(org-refile-targets '((nil :maxlevel . 10) (org-agenda-files :maxlevel . 10)))
   '(org-refile-use-cache t)
   '(org-refile-use-outline-path 'buffer-name)
   '(org-startup-folded 'nofold)
   '(org-startup-shrink-all-tables t)
   '(org-sticky-header-always-show-header nil)
   '(org-sticky-header-full-path 'reversed)
   '(org-stuck-projects '("+LEVEL<=2/!" ("TODO" "MAYBE" "INPROGRESS") nil ""))
   '(org-tag-persistent-alist '(("daily" . 100) ("weekly" . 119)))
   '(org-todo-keywords
     '((sequence "TODO(t)" "MAYBE(m/!)" "INPROGRESS(p!)" "STALLED(a)" "BLOCKED(k@/!)"
                 "INREVIEW(r!)" "|" "DONE(d/@)" "DELEGATED(g@)" "DEFERRED(e!)"
                 "NOTDONE(n)")
       (type "BUG(b/!)" "CLEANUP(l/!)" "|" "FIXED(x/@)" "WONTFIX(w@/@)")
       (type "ENHANCEMENT(h/!)" "FEATURE(f/!)" "|" "RELEASED(s/@)" "CANCELED(c/@)")))
   '(org-use-effective-time t)
   '(orgit-rev-description-format "%%N (magit-rev %%R): %s")
   '(paradox-github-token t nil nil "Disable GitHub integration in Paradox")
   '(pdf-tools-installer-os "nixos")
   '(projectile-ignored-project-function 'bcc32/projectile-ignored-project-function)
   '(revert-without-query '(""))
   '(rustic-default-test-arguments "--quiet --benches --tests --all-features")
   '(rustic-format-trigger 'on-compile)
   '(rustic-lsp-client 'eglot)
   '(safe-local-variable-values
     '((auto-insert-alist
        (rust-mode "" "// " '(setq v1 (read-from-minibuffer "Title: ")) v1 n "// "
                   '(setq v2 (read-from-minibuffer "URL: ")) v2 n n
                   "use super::def::*;" n n _ n n "#[cfg(test)]" n "mod tests {" n
                   "use super::*;" n "use test_case::test_case;" n n
                   "#[test_case(args => result)]" n "fn test(args: Ty) -> Ty {" n
                   "}" n "}" n))
       (diff-add-log-use-relative-names . t)
       (dired-guess-shell-alist-user ("\\.zip\\'" "../bin/import.sh"))
       (etags-regen-ignores "test/manual/etags/")
       (etags-regen-regexp-alist
        (("c" "objc") "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
         "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
       (flycheck-mode) (javascript-backend . lsp) (javascript-backend . tern)
       (javascript-backend . tide)
       (lsp-rust-analyzer-diagnostics-disabled . ["unresolved-proc-macro"])
       (magit-todos-exclude-globs ".git/" ".vendor/") (magit-todos-mode)
       (magit-todos-rg-extra-args "--hidden")
       (org-archive-location . "archive/%s_archive::") (org-table-copy-increment)
       (projectile-project-compilation-cmd . "make -O -j")
       (projectile-project-compilation-cmd
        . "nix run 'nixpkgs#cmake' -- -S . && make && make -C test test-batch")
       (typescript-backend . lsp) (typescript-backend . tide)
       (vc-git-annotate-switches . "-w") (vc-prepare-patches-separately)
       (whitespace-style)))
   '(save-abbrevs 'silently)
   '(save-some-buffers-default-predicate 'save-some-buffers-root)
   '(send-mail-function 'smtpmail-send-it)
   '(sentence-end-double-space t)
   '(sh-basic-offset 2)
   '(shell-file-name (executable-find "bash"))
   '(shell-pop-term-shell explicit-shell-file-name nil (shell) "prevent Spacemacs shell layer from overriding the defcustom default")
   '(shfmt-arguments '("-i" "2" "-bn" "-s"))
   '(shr-use-colors nil)
   '(smtpmail-debug-info t)
   '(smtpmail-queue-dir "~/mail/queue/")
   '(smtpmail-servers-requiring-authorization "smtp[.]fastmail[.]com")
   '(smtpmail-smtp-server "smtp.fastmail.com")
   '(smtpmail-smtp-service 465)
   '(smtpmail-smtp-user "z@bcc32.com")
   '(smtpmail-stream-type 'ssl)
   '(solarized-distinct-doc-face t)
   '(solarized-use-more-italic t)
   '(spacemacs-keep-legacy-current-buffer-delete-bindings nil)
   '(spacemacs-large-file-modes-list
     '(org-mode ledger-mode archive-mode tar-mode jka-compr git-commit-mode
                image-mode doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                pdf-view-mode tags-table-mode fundamental-mode))
   '(sql-product 'sqlite)
   '(switch-to-buffer-in-dedicated-window 'pop)
   '(switch-to-buffer-obey-display-actions t)
   '(tab-always-indent 'complete)
   '(tab-width 8)
   '(tao-theme-use-sepia nil)
   '(terminal-here-linux-terminal-command 'urxvt)
   '(terminal-here-mac-terminal-command 'iterm2)
   '(tramp-use-connection-share nil nil nil "Configure ControlMaster in ~/.ssh/config instead")
   '(tuareg-prettify-symbols-full t)
   '(typescript-indent-level 2)
   '(undo-no-redo t)
   '(undo-tree-auto-save-history nil)
   '(use-package-compute-statistics t)
   '(user-mail-address "z@bcc32.com")
   '(vc-follow-symlinks t)
   '(vc-handled-backends '(Git Hg))
   '(what-cursor-show-names t)
   '(zoneinfo-style-world-list
     '(("America/Los_Angeles" "Seattle") ("America/New_York" "New York")
       ("Europe/London" "London") ("Europe/Amsterdam" "Amsterdam")
       ("Asia/Hong_Kong" "Hong Kong") ("Asia/Shanghai" "Shanghai")
       ("Asia/Tokyo" "Tokyo"))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-link ((t (:inherit link :underline t))))
   '(tuareg-font-lock-attribute-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
   '(tuareg-font-lock-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic))))
   '(tuareg-font-lock-infix-extension-node-face ((t (:inherit font-lock-preprocessor-face :slant italic)))))
  )
