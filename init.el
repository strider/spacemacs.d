;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
   '(html
     rust
     lua
     systemd
     multiple-cursors
     pass
     (ibuffer :variables ibuffer-group-buffers-by 'projects)

     (ivy :variables ivy-enable-advanced-buffer-information t)
     fasd
     ranger
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete)
     better-defaults
     emacs-lisp
     cscope
     (git :variables
          git-magit-status-fullscreen t
          git-gutter-use-fringe t)

     (colors :variables
             colors-enable-nyan-cat-progress-bar nil
             colors-default-rainbow-identifiers-sat t)
     github
     markdown
     (org :variables
          org-enable-bootstrap-support t
          org-want-todo-bindings t
          org-enable-reveal-js-support t
          org-projectile-file "~/org/TODOs.org")
     (shell :variables
            shell-default-height  0
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-default-term-shell "/bin/zsh")
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
                     ;; spell-checking-enable-auto-dictionary t
                     ;; =enable-flyspell-auto-completion= t)

     syntax-checking
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-tool 'git-gutter+)
     yaml
     (python :variables
             python-sort-imports-on-save nil
             python-fill-column 80)
     shell-scripts
     major-modes
     ansible
     themes-megapack
     docker
     unicode-fonts
     (mu4e :variables
           mu4e-account-alist t
           mu4e-enable-async-operations t)
     (elfeed :variables rmh-elfeed-org-files (list "~/.spacemacs.d/elfeed.org"))
     (multiple-cursors :variables multiple-cursors-backend 'evil-mc)
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(git-gutter
                                      magit-find-file
                                      company-ansible
                                      company-anaconda
                                      restclient
                                      ansible-doc
                                      forge
                                      all-the-icons-dired
                                      ivy-yasnippet
                                      keychain-environment
                                      mu4e-jump-to-list
                                      pyvenv
                                      rpm-spec-mode
                                      ;; (magit-gerrit2 :location "~/DEV/work/git/laptop_config/EMACS/magit-gerrit2/")
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    magithub
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
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   ;; dotspacemacs-startup-banner '000
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists nil
   ;; dotspacemacs-startup-lists '((recents . 5)
   ;;                              (projects . 7)
   ;;                              (agenda . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         smyx
                         farmhouse-dark
                         material
                         naquadah
                         solarized-dark
                         solarized-light
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t


   ;; Default font, or prioritized list of fons. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco for Powerline"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.5
                               )

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'

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
   dotspacemacs-distinguish-gui-tab nil

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
   dotspacemacs-which-key-delay 0.2

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
   dotspacemacs-loading-progress-bar nil

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
   dotspacemacs-mode-line-unicode-symbols t

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
   dotspacemacs-line-numbers 'relative

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
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

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
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

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

  ;; (keychain-refresh-environment)
  (setq custom-file "~/.spacemacs.d/custom.el")

  (setq initial-buffer-choice t)
  (add-to-list 'exec-path "~/bin")
  (add-to-list 'exec-path "~/.local/bin")

  (setq-default auto-fill-function 'do-auto-fill)

  (setq git-commit-fill-column 72)

  ;; Window numbering should start from "1" for each new frame.
  (setq-default winum-scope 'frame-local)
  (setq-default line-number-mode 'relatives)

  )

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

  ;; (if (display-graphic-p)
  ;;     (load-theme 'solarized-dark t)
  ;;   (load-theme 'smyx t))

  (global-git-commit-mode t)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-alh")

  (setq yas-snippet-dirs '("~/.spacemacs.d/snippets/"))

  ;; (load-file (expand-file-name "conf/magit.el"
  ;;                              "~/.spacemacs.d"))
  (setq git-commit-summary-max-length 80)
  (setq magit-save-some-buffers nil)
  (setq magit-remote-ref-format 'remote-slash-branch)
  (setq magit-completing-read-function 'ivy-completing-read)

  (add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0))) ; Disable it
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)

  (global-set-key (kbd "<f3>") 'magit-status)
  (global-set-key (kbd "C-x p") 'magit-find-file-completing-read)

  ;; (require 'magit-gerrit2)
  ;; (setq-default magit-gerrit2-ssh-creds "gchamoul@review.openstack.org")
  ;; (setq-default magit-gerrit2-remote "gerrit")

  (setq magit-repository-directories
        '(("~/DEV/work/git/tripleo/UPSTREAM" . 2)
          ("~/DEV/work/git/tripleo/OOOQ/" . 2)
          ("~/DEV/work/git/ansible/" . 2)
          ("~/DEV/work/git/kni/" . 2)))

  (add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0))) ; Disable it
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (setq git-commit-summary-max-length 80)

  (setq magit-commit-signoff t)
  (setq magit-save-some-buffers nil)
  (setq magit-remote-ref-format 'remote-slash-branch)

  (global-set-key (kbd "C-x p") 'magit-find-file-completing-read)

  ;; (spacemacs/toggle-highlight-long-lines-globally-on)

  (global-spacemacs-whitespace-cleanup-mode t)

  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'python-mode-hook 'anaconda-mode)

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda :with company-capf)))
  (global-auto-complete-mode t)

  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c i") 'counsel-imenu)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-c h") 'counsel-locate)
  (global-set-key (kbd "C-c n") 'counsel-ibuffer)
  (global-set-key (kbd "C-c o") 'counsel-org-capture)
  (global-set-key (kbd "C-c r") 'ivy-recentf)
  (global-set-key (kbd "C-c s") 'ivy-yasnippet)
  (global-set-key (kbd "C-c m") 'man)
  (global-set-key (kbd "<C-f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<C-f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<C-f1> l") 'counsel-find-library)

  (require 'yasnippet)
  (yas-global-mode 1)
  (setq yas-indent-line 'fixed)

  (add-hook 'text-mode-hook #'abbrev-mode)
  (setq-default save-abbrevs 'silent)
  (setq-default abbrev-file-name "~/.spacemacs.d/abbrev_defs")
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;; `org-indent-mode' does not play nice with git-gutter, so let's disable
  ;; it :-(
  (setq git-gutter:disabled-modes '(org-mode))

  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  (add-hook 'yaml-mode-hook #'ansible-doc-mode)

  ;; gitgutter
  (global-git-gutter-mode +1)

  (define-key evil-normal-state-map (kbd "Q") (kbd "gqip"))
  (setq sentence-end-double-space t)

  (setq projectile-completion-system 'ivy)
  (setq python-shell-interpreter "python3")

  (setq calendar-week-start-day 1)	; Weeks start on monday
  (setq calendar-date-style 'european)
  (setq european-calendar-style t)

  (setq calendar-legal-holidays
        '((holiday-fixed 1 1 "New Year's Day")
          (holiday-fixed 5 1 "Fête du Travail")
          (holiday-fixed 5 8 "Armistice 1945")
          (holiday-fixed 7 14 "Fête National")
          (holiday-fixed 8 15 "Assomption")
          (holiday-fixed 11 1 "Toussaint")
          (holiday-fixed 11 11 "Armistice 1918")
          (holiday-fixed 12 25 "Christmas")
          (holiday-easter-etc 1 "Lundi de Pâques")
          (holiday-easter-etc 39 "Ascension")
          (holiday-easter-etc 50 "Lundi de Pentecôte")))

  (setq calendar-celebration-holidays
        '((holiday-fixed 2 2 "Chandeleur")
          (holiday-fixed 2 14 "Fête des amoureux")
          (holiday-float 3 0 1 "Fête des grands-mères")
          (holiday-fixed 3 17 "St. Patrick's Day")
          (holiday-fixed 4 1 "April Fools' Day")
          (holiday-float 5 0 -1 "Fête des Mères")
          (holiday-float 6 0 3 "Fête des Pères")
          (holiday-fixed 6 21 "Fête de la musique")
          (holiday-fixed 10 31 "Halloween")
          (holiday-easter-etc -47 "Mardi Gras")))

  (setq calendar-holidays
        `(,@holiday-solar-holidays
          ,@calendar-legal-holidays
          ,@calendar-celebration-holidays))
  (customize-set-variable 'org-journal-dir "~/org/journal/")
  (customize-set-variable 'org-journal-date-format "%A, %d %B %Y")

  (setq org-directory "~/org")
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (setq org-startup-indented t)
  (setq org-completion-use-ido t)
  (setq org-log-done t)

  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "STARTED(s!)"
                    "DELEGATED(g@)"
                    "BLOCKED(b@)"
                    "FEEDBACK(f!/@)"
                    "REWORK(r!/!)"
                    "VERIFY(v/!)"
                    "|"
                    "DONE(d!)"
                    "CANCELED(c@)")
          (sequence "PROJECT(j!)" "|" "CANCELED(c@)" "DONE(d!)")))

  (setq org-todo-keyword-faces
        '(("DELEGATED" . shadow)
          ("STARTED" . org-warning)
          ("FEEDBACK" . (:foreground "yellow"))
          ("BLOCKED" . shadow)
          ("REWORK" . org-warning)
          ("VERIFY" . org-warning)
          ("CANCELED" . (:foreground "red" :weight bold :strike-through t))))

  (setq org-clock-persist-query-save t)
  (setq org-src-preserve-indentation t)
  (setq org-enforce-todo-dependencies t)
  (setq org-link-abbrev-alist
        '(("colissimo" . "http://www.coliposte.net/particulier/suivi_particulier.jsp?colispart=")
          ("launchpad" . "https://bugs.launchpad.net/bugs/")
          ("review"    . "https://review.openstack.org/#/c/")
          ("rhbz"      . "https://bugzilla.redhat.com/show_bug.cgi?id=")))

  (setq ibuffer-modified-char ?✍)
  (setq ibuffer-read-only-char ?✗)
  (setq ibuffer-marked-char ?✓)
  (setq ibuffer-deletion-char ?␡)
  (setq ibuffer-show-empty-filter-groups nil)

  (display-time-mode 1)
  (global-hi-lock-mode 1)
  (global-git-gutter+-mode 1)
  (global-color-identifiers-mode 1)
  (windmove-default-keybindings)    ; Move between frames with Shift+arrow
  (setq mouse-yank-at-point t)

  (with-eval-after-load 'org
    (setq org-capture-templates
          '(("t" "todo entry human" entry
             (file+headline "~/org/inbox.org" "TASKS")
             "* TODO %i%? \n %u")
            ("a" "TODO ADMIN Tasks" entry
             (file+headline "~/org/personal.org" "ADMIN TASKS")
             "* TODO %i%? \n %u")
            ("e" "TODO PERSONAL Tasks" entry
             (file+headline "~/org/personal.org" "PERSONAL TASKS")
             "* TODO %i%? \n %u")
            ))

    (setq org-agenda-files '("~/org/inbox.org"
                             "~/org/personal.org"))
    (setq org-default-notes-file "~/org/inbox.org")
    )

  (setq-default
   ad-redefinition-action 'accept                   ; Silence warnings for redefinition
   cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
   display-time-default-load-average nil            ; Don't display load average
   fill-column 80                                   ; Set width for automatic line breaks
   help-window-select t                             ; Focus new help windows when opened
   inhibit-startup-screen t                         ; Disable start-up screen
   initial-scratch-message ""                       ; Empty the initial *scratch* buffer
   kill-ring-max 128                                ; Maximum length of kill ring
   load-prefer-newer t                              ; Prefers the newest version of a file
   mark-ring-max 128                                ; Maximum length of mark ring
   scroll-conservatively most-positive-fixnum       ; Always scroll by one line
   select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
   tab-width 4                                      ; Set width for tabs
   use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
   vc-follow-symlinks t                             ; Always follow the symlinks
   view-read-only t)                                ; Always open read-only buffers in view-mode
  (cd "~/")                                         ; Move to the user directory
  (column-number-mode 1)                            ; Show the column number
  (display-time-mode 1)                             ; Enable time in the mode-line
  (fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
  (global-hl-line-mode 1)                           ; Hightlight current line
  (set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
  (show-paren-mode 1)                               ; Show the parent
  (indent-guide-global-mode)
  (xterm-mouse-mode -1)


  (load "~/.spacemacs.d/mutt.el")
  (add-to-list 'auto-mode-alist '(".*neomutt.*" . mutt-mode))
  (setq mail-header-separator "")

  (setq mu4e-maildir "~/Mail/redhat-gmail")
  (setq mu4e-trash-folder "/Trash")
  (setq mu4e-refile-folder "/Archive")
  (setq mu4e-sent-folder "/Sent")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-compose-signature-auto-include nil)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-use-gnus nil)
  (setq mu4e-view-show-addresses t)
  ;; (setq mu4e-update-interval 300)
  (setq user-mail-address "gchamoul@redhat.com")
  (setq user-full-name "Gaël Chamoulaud")
  (setq mu4e-reply-to-address "gchamoul@redhat.com")

  ;; (setq mu4e-enable-notifications t)

  ;; (with-eval-after-load 'mu4e-alert
  ;;   (mu4e-alert-set-default-style 'notifications))

  ;; (mu4e-alert-enable-mode-line-display)
  ;; (mu4e-alert-enable-notifications)
  ;; (setq mu4e-enable-mode-line t)

  ;; (setq mu4e-alert-interesting-mail-query
  ;;       (concat
  ;;        "flag:unread"
  ;;        " AND NOT flag:trashed"
  ;;        " AND maildir:/Inbox"
  ;;        " OR"
  ;;        "flag:unread"
  ;;        " AND NOT flag:trashed"
  ;;        " AND maildir:/ML.OST-RH-OPENSTACK-UI"))

  (setq mu4e-headers-fields
        '((:human-date    .  12)
          (:flags         .   6)
          (:from          .  40)
          (:subject       . nil)))

  (setq  mu4e-maildir-shortcuts
         '(("/INBOX"            . ?i)
           ("/ML.announce-list" . ?a)
           ("/ML.cdg-list"      . ?c)
           ("/ML.France-list"   . ?f)
           ("/Perso"            . ?p)
           ("/ML.memo-list"     . ?m)
           ("/ML.KNI-DEVEL"     . ?k)
           ("/ML.OST-UPSTREAM-TRIPLEO-DEV"     . ?t)))

    ;;; Bookmarks
  (setq mu4e-bookmarks
        `(("maildir:/INBOX" "Inbox" ?i)
          ("list:openstack-discuss.lists.openstack.org and subject:tripleo" "OOO-dev" ?o)
          ("maildir:/ML.OST-RH-OPENSTACK-UI" "RHOSPUI-internal"      ?s)
          ("maildir:/ML.OST-RH-OPENSTACK-DEVEL" "RHOS-dev"       ?v)
          ("maildir:/ML.OST-RH-OPENSTACK-PGM" "RHOS-pgm"         ?g)
          ("maildir:/ML.OST-RHOS-TECH" "RHOS-tech"         ?c)
          ("maildir:/ML.memo-list" "memo-list"         ?m)
          ("maildir:/ML.Friday-list" "friday-list"         ?f)
          ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?a)))

    ;;; Sending emails ...
  (setq message-sendmail-f-is-evil 't)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))

    ;;; Global parameters
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-confirm-quit nil)
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-headers-thread-child-prefix '("├>" . "├▶ "))
  (setq mu4e-headers-thread-last-child-prefix '("└>" . "└▶ "))
  (setq mu4e-headers-thread-connection-prefix '("│" . "│ "))
  (setq mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ "))
  (setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶ "))

  (setq mu4e-use-fancy-chars nil)
  (setq mu4e-split-view 'horizontal)

  (setq mu4e-headers-auto-update t)
  (setq mu4e-headers-include-related t)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-view-prefer-html nil)
  (setq mu4e-html2text-command "w3m -dump -T text/html")
  (setq mu4e-completing-read-function 'ivy-completing-read)
  ;; Use imagemagick, if available.
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
  (global-set-key (kbd "<f5>") 'mu4e-update-index)
  (global-set-key (kbd "<f7>") 'mu4e-jump-to-list)

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(company-quickhelp-color-background "#3E4452")
 '(company-quickhelp-color-foreground "#ABB2BF")
 '(custom-safe-themes
   (quote
    ("aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(display-line-numbers-type (quote relative))
 '(display-time-mode t)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#37474f")
 '(global-display-line-numbers-mode t)
 '(gnus-logo-colors (quote ("#528d8d" "#c0c0c0")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (nimbus-theme telephone-line zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode wolfram-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe vala-snippets vala-mode uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-evil toxi-theme toc-org thrift tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stan-mode spotify spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode scad-mode sass-mode reverse-theme restclient restart-emacs rebecca-theme ranger rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme qml-mode pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin planet-theme pkgbuild-mode pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pdf-tools pcre2el password-generator paradox ox-twbs overseer orgit organic-green-theme org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme nameless mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme matlab-mode material-theme markdown-toc majapahit-theme magit-svn magit-gitflow magit-find-file madhat2r-theme macrostep lush-theme lorem-ipsum logcat live-py-mode link-hint light-soap-theme kivy-mode kaolin-themes jinja2-mode jbeans-theme jazz-theme ir-black-theme insert-shebang inkpot-theme indent-guide importmagic impatient-mode ibuffer-projectile hungry-delete hoon-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-spotify-plus helm-pydoc helm-purpose helm-projectile helm-pass helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-cscope helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md gandalf-theme fuzzy forge font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-bashate flx-ido flatui-theme flatland-theme fish-mode fill-column-indicator fasd farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig ebuild-mode dumb-jump dracula-theme dotenv-mode doom-themes doom-modeline django-theme discover-my-major diminish diff-hl define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme dactyl-mode cython-mode cyberpunk-theme counsel-projectile company-web company-statistics company-shell company-go company-ansible company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode clues-theme clean-aindent-mode cherry-blossom-theme centered-cursor-mode busybee-theme bubbleberry-theme browse-at-remote birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile arduino-mode apropospriate-theme anti-zenburn-theme ansible-doc ansible ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adoc-mode ace-link ace-jump-helm-line ac-ispell)))
 '(send-mail-function (quote mailclient-send-it))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
