;;; init.el --- bcc32's Spacemacs configuration  -*- lexical-binding: t -*-

;; https://github.com/bcc32/dotfiles

;;; Commentary:

;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;; Code:

(require 'seq)

(defun when-any-installed (executable-names pkgs)
  "Only enable PKGS if any of EXECUTABLE-NAMES is installed.

A name is considered installed if `executable-find' returns non-nil."
  (if (seq-some #'executable-find executable-names)
      pkgs))

(defun when-on-hostname (name pkgs)
  "Only enable PKGS if current machine's hostname is NAME."
  (if (string= (system-name) name)
      pkgs))

(defun when-on-type (type pkgs)
  "Only enable PKGS if `system-type' is `eql' to TYPE."
  (if (eql system-type type)
      pkgs))

;; TODO conditionally reimplement this in Ecaml
(defun bcc32/ansi-color-region-or-buffer ()
  "Render ANSI SGR color sequences in the current region if it is active.

Otherwise, render sequences in the current buffer."
  (interactive)
  (require 'ansi-color)
  (with-silent-modifications
    (save-restriction
      (when (use-region-p) (narrow-to-region (region-beginning) (region-end)))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(with-eval-after-load 'ledger-mode
  (defconst bcc32/ledger-posting-effective-date-regexp
    (rx ";" (one-or-more space) "[=" (group (regexp ledger-iso-date-regexp)) "]")
    "A comment containing an effective date for a posting.")

  (defun bcc32-ledger-should-insert-effective-date ()
    (let ((end (save-excursion (ledger-navigate-end-of-xact) (point)))
          accounts
          account)
      (beginning-of-line)
      (when (ledger-next-account end)
        (setq account (match-string 1)))
      (ledger-navigate-beginning-of-xact)
      (while (ledger-next-account end)
        (push (match-string 1) accounts))
      (not (or (string= account "Assets:Cash:Wallet")
               (seq-some (lambda (account) (string-match-p (rx bos "Income:Work:") account))
                         accounts)))))

  (defun bcc32/ledger-promote-effective-date ()
    "Move the effective date for a posting in this transaction to the transaction."
    (interactive)
    (let ((end (ledger-navigate-end-of-xact)))
      (ledger-navigate-beginning-of-xact)
      (unless (re-search-forward bcc32/ledger-posting-effective-date-regexp end t)
        (error "No effective date in transaction"))
      (when (re-search-forward bcc32/ledger-posting-effective-date-regexp end t)
        (error "Multiple effective dates in transaction"))
      (let ((effective-date (match-string 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (ledger-navigate-beginning-of-xact)
        (re-search-forward ledger-iso-date-regexp)
        (insert "=" effective-date)
        (ledger-toggle-current))))

  (defun bcc32/ledger-yank-code ()
    "Insert a code for the current transaction from the kill ring."
    (interactive)
    (save-excursion
      (ledger-navigate-beginning-of-xact)
      (unless (looking-at ledger-iterate-regexp)
        (user-error "Not inside xact"))
      (goto-char (match-beginning ledger-regex-iterate-group-payee))
      (when (ledger-regex-iterate-code)
        (delete-region (1- (match-beginning ledger-regex-iterate-group-code))
                       (point)))
      (let ((code (current-kill 0)))
        (insert (format "(%s) " (string-trim code)))))))

(defun bcc32/add-link (url tags)
  "Add URL from clipboard to pocket reader, prompting for TAGS."
  (interactive
   (let* ((url (progn
                 (require 'org-web-tools)
                 (or (org-web-tools--get-first-url)
                     (user-error "No URL found in kill-ring"))))
          (tags (read-from-minibuffer (format "Tags for %s: " url) nil nil nil
                                      'bcc32/add-link-history)))
     (list url tags)))
  (require 'pocket-lib)
  (when (pocket-lib-add-urls (list url) :tags tags)
    (message "Added %s" url)))

;; TODO: Perhaps this should be upstreamed?
(defun bcc32/pocket-reader-browse ()
  "Open marked or current items in external browser.
The `browse-url' function is used."
  (interactive)
  (pocket-reader-open-url
   :fn (lambda (&rest args)
         (apply #'browse-url args)
         ;; Return t because the browsing function may not return non-nil
         ;; when it succeeds, preventing the item from being archived
         t)))

(defun bcc32/browse-url-on-ssh-client-if-exists (url &rest args)
  "Browse URL on the selected frame's $SSH_CONNECTION, if it exists.

Fall back to `browse-url-default-browser' if SSH_CONNECTION is
unset in the selected frame, passing ARGS."
  (if (getenv "SSH_CONNECTION" (selected-frame))
      (let ((process-environment (append (frame-parameter nil 'environment)
                                         process-environment)))
        (call-process (expand-file-name "~/bin/browse-on-ssh-client")
                      nil nil nil
                      url))
    (apply #'browse-url-default-browser url args)))

(defun bcc32/browse-url-on-wsl (url &rest args)
  "Browse URL on the host Windows machine, if running in WSL.

Fall back to `browse-url-default-browser' if cmd.exe is not
found, passing ARGS."
  (if-let (cmd (executable-find "cmd.exe"))
      (call-process cmd nil nil nil
                    "/c"
                    (format "start /wait /b %s" url))
    (apply #'browse-url-default-browser url args)))

(defun bcc32/copy-region-as-kill-refill-for-web (beg end &optional region)
  "Copy the region to the kill ring, refilling the text for web use.

When pasting into web forms, newlines separate paragraphs, unlike
in Emacs where paragraphs are delimited by empty lines.  This
command fills the copied text with no newlines within paragraphs.

Use the region between BEG and END.  When called from Lisp and
REGION is non-nil, use the current region instead of BEG and
END."
  (interactive (list (mark) (point) (prefix-numeric-value current-prefix-arg)))
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
    (copy-region-as-kill beg end region)))

(defun bcc32/projectile-ignored-project-function (project-root)
  "Return t if a project rooted at PROJECT-ROOT should be ignored by projectile."
  (string-prefix-p "/nix/store/" project-root))

(defun bcc32//set-fill-column-in-text-mode-hook ()
  "Set `fill-column' to 70 characters in derived modes of `text-mode'."
  (setq-local fill-column 70))

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
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path
   `(,(expand-file-name "layers/" dotspacemacs-directory))

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
     (syntax-checking :variables
                      syntax-checking-use-original-bitmaps t)

     ;; Completion
     (auto-completion :variables
                      auto-completion-idle-delay 1.0)
     (ivy :variables
          ivy-enable-advanced-buffer-information t)

     ;; Emacs
     helpful
     ibuffer
     (org :variables
          org-enable-appear-support t
          org-enable-github-support t
          org-enable-notifications t
          org-projectile-file "TODO.org"
          org-start-notification-daemon-on-startup t
          org-want-todo-bindings t)
     (unicode-fonts :variables
                    ;; https://github.com/mickeynp/ligature.el/issues/10
                    ;; https://github.com/mickeynp/ligature.el/issues/28
                    unicode-fonts-enable-ligatures (>= emacs-major-version 28))

     ;; Programming and markup languages
     autohotkey
     c-c++
     csv
     ,(when-any-installed '("sbcl") 'common-lisp)
     emacs-lisp
     ,(when-any-installed '("go")
                          '(go :variables
                               go-format-before-save t
                               go-tab-width 4))
     ,(when-any-installed '("ghc" "stack") 'haskell)
     html
     (javascript :variables
                 javascript-backend 'tide
                 javascript-fmt-on-save t
                 javascript-fmt-tool 'prettier)
     json
     jsonnet
     ,@(when-any-installed '("xelatex" "lualatex" "pdflatex" "latexmk")
                           '(bibtex
                             latex))
     markdown
     (ocaml :variables
            ocaml-format-on-save t)
     perl5
     ,(when-any-installed '("python" "python3")
                          '(python :variables
                                   python-format-on-save t
                                   python-formatter 'black
                                   python-sort-imports-on-save t))
     ,(when-any-installed '("ruby") 'ruby)
     ,(when-any-installed '("rustc" "cargo") 'rust)
     ,(when-any-installed '("chicken" "guile")
                          '(scheme :variables
                                   scheme-implementations '(chicken guile)))
     (shell-scripts :variables
                    shell-scripts-format-on-save t)
     sql
     (typescript :variables
                 typescript-backend 'tide
                 typescript-fmt-on-save t
                 typescript-fmt-tool 'prettier)
     yaml

     ;; Operating systems
     (nixos :variables
            nixos-format-on-save t)
     osx

     ;; Source control
     git
     (version-control :variables
                      version-control-diff-tool nil)

     ;; Spacemacs distribution layers
     spacemacs-completion
     spacemacs-layouts
     spacemacs-editing
     spacemacs-editing-visual
     spacemacs-evil
     spacemacs-language
     spacemacs-misc
     spacemacs-modeline
     spacemacs-navigation
     spacemacs-org
     spacemacs-project
     spacemacs-purpose
     spacemacs-visual

     ;; Tools
     command-log
     fasd
     ,(when-any-installed '("ledger" "hledger") 'finance)
     (shell :variables
            shell-enable-smart-eshell t)

     ;; Web services
     pocket
     spotify

     ;; User layers
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
   '((advent-of-code :location (recipe :fetcher github
                                       :repo "bcc32/advent-of-code"
                                       :files ("elisp/advent-of-code.el")))
     base16-theme
     solarized-theme
     tao-theme
     zenburn-theme
     (direnv :toggle (executable-find "direnv"))
     mode-line-bell
     (pocket-reader :location (recipe :fetcher github
                                      :repo "bcc32/pocket-reader.el"
                                      :branch "working")))

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

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
   dotspacemacs-startup-banner 'official

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
   dotspacemacs-mode-line-theme '(spacemacs :separator arrow :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
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
   dotspacemacs-large-file-size 10

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

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

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
   '(:visual t
     :enabled-for-modes prog-mode
                        text-mode
     :disabled-for-modes org-mode)

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

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
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

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session.  By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (when (eq system-type 'darwin)
    (spacemacs/load-spacemacs-env)))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq source-directory (expand-file-name "~/src/emacs/"))
  (define-advice package--save-selected-packages (:around (&rest _) dont-save-to-custom-file 100)
    "Don't save `package-selected-packages' to `custom-file'.")
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file)
  (add-to-list 'load-path dotspacemacs-directory))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration.  You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (put 'list-timers 'disabled nil)

  (bind-key "C-x C-c" (lambda () (interactive) (error "Do not use C-x C-c")))

  ;; Superword mode, for evil
  (with-eval-after-load 'evil
    (defalias 'forward-evil-word #'forward-evil-symbol))

  ;; toggles
  (spacemacs/toggle-mode-line-org-clock-on)
  (spacemacs/toggle-mode-line-version-control-off)

  (setf (alist-get t ivy-format-functions-alist)
        #'ivy-format-function-line)

  (spacemacs/set-leader-keys
    "Ca" 'bcc32/ansi-color-region-or-buffer)

  (spacemacs/set-leader-keys-for-major-mode 'ledger-mode
    "j" 'bcc32/ledger-promote-effective-date
    "y" 'bcc32/ledger-yank-code)

  (with-eval-after-load 'ledger-mode
    (add-hook 'ledger-mode-hook #'turn-off-auto-fill))

  (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode
    "v" 'merlin-enclosing-expand
    "f" 'ocamlformat)

  (define-advice ocamlformat (:after () run-ocp-indent)
    (ocp-indent-buffer))

  ;; Workaround for https://github.com/syl20bnr/spacemacs/pull/14070
  (with-eval-after-load 'magit
    (setq magit-git-executable "git"))

  (spacemacs/set-leader-keys "gy" 'bcc32-org-commit-and-push-all)

  (define-advice org-web-tools-read-url-as-org (:after (&rest _) disable-org-indent-mode)
    (org-indent-mode -1))

  (with-eval-after-load 'pocket-reader
    (bind-key "b" 'bcc32/pocket-reader-browse pocket-reader-mode-map))

  (add-hook 'text-mode-hook #'bcc32//set-fill-column-in-text-mode-hook)
  (add-hook 'text-mode-hook #'turn-on-auto-fill)

  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (require 'bcc32-abbrev)
  (bcc32-ocaml-abbrevs)

  (define-advice recentf-cleanup (:after (&rest _) cleanup-file-name-history)
    "Clean up `file-name-history' after cleaning up recentf lists."
    (setq file-name-history
          (cl-delete-if (lambda (file)
                          (or (file-remote-p file) (not (file-exists-p file))))
                        file-name-history)))

  ;; Make sure my customizations take precedence over settings that Spacemacs
  ;; `setq's, even after running `dotspacemacs/sync-configuration-layers'.
  ;;
  ;; This would not be necessary if my customizations were in this file instead
  ;; of a separate file.
  (load custom-file))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init.el ends here
