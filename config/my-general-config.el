;; My General Configuration

(message "------------------------------------------------------")
(message "strider's emacs start to init...")

(set-language-environment "UTF-8")

(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'yaml-mode-hook #'display-line-numbers-mode)
;; (add-hook 'text-mode-hook #'display-line-numbers-mode)
(setq-default
 display-line-numbers-current-absolute nil        ; Current line is 0
 display-line-numbers-type 'relative              ; Prefer relative numbers
 display-line-numbers-width 2)                    ; Enforce width to reduce computation

(setq alert-default-style 'notifications)

(display-battery-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs text rendering optimizations
;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

;; Only render text left to right
(setq-default bidi-paragraph-direction 'left-to-right)

;; Disable Bidirectional Parentheses Algorithm
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

;; Files with known long lines
;; SPC f l to open files literally to disable most text processing

;; So long mode when Emacs thinks a file would affect performance
(if (version<= "27.1" emacs-version)
    (global-so-long-mode 1))

;; End of: Emacs text rendering optimizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/syl20bnr/spacemacs/issues/2705
;; (setq tramp-mode nil)
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(beacon-mode 1)

(setq-default truncate-lines 1)
(setq warning-minimum-level :error)
(setq package-check-signature nil)
(setq initial-buffer-choice t)
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/.local/bin")

(setq-default auto-fill-function 'do-auto-fill)

(defun strider/flyspell-and-whitespace-mode ()
  "Toggle `flyspell-mode' and `whitespace-mode'."
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
    (flyspell-mode)
    (when flyspell-mode
      (flyspell-buffer)))
  (whitespace-mode 'toggle))

(global-set-key (kbd "<f9>") #'strider/flyspell-and-whitespace-mode)

(olivetti-mode 0)

(defun strider/enter-focus-mode ()
  (interactive)
  (olivetti-mode 1))

(defun strider/leave-focus-mode ()
  (interactive)
  (olivetti-mode 0))

(defun strider/toggle-focus-mode ()
  (interactive)
  (if (symbol-value olivetti-mode)
      (strider/leave-focus-mode)
    (strider/enter-focus-mode)))

(global-set-key (kbd "<f8>") #'strider/toggle-focus-mode)

(setq git-commit-fill-column 72)

;; ;; Window numbering should start from "1" for each new frame.
(setq-default winum-scope 'frame-local)

;; show the filepath in the frame title
;; http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
                  "%b"))))

;; (add-hook 'text-mode-hook 'flyspell-mode)
;; Disable line numbers for some modes
;; (dolist (mode '(prog-mode-hook
;;                 python-mode-hook
;;                 term-mode-hook
;;                 shell-mode-hook
;;                 treemacs-mode-hook
;;                 eshell-mode-hook))
;;   (add-hook mode (lambda () (flyspell-mode 0))))

(with-eval-after-load "ispell"
  (setq ispell-program-name "hunspell")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "fr_FR,en_US,en_GB")
  (setq ispell-dictionary "fr_FR,en_US,en_GB"))

;; colorized dired https://github.com/purcell/diredfl
(diredfl-global-mode t)
(setq-default indent-tabs-mode nil)
(cua-mode t)
(global-evil-mc-mode  1)
(setq dired-recursive-deletes 'always)
(setq dired-listing-switches "-alh")

;;Line Spacing by default
;; (setq-default line-spacing 2)

(setq yas-snippet-dirs '("~/.spacemacs.d/snippets/"))

(global-git-commit-mode t)
(setq git-commit-summary-max-length 80)
(setq magit-save-some-buffers nil)
(setq magit-remote-ref-format 'remote-slash-branch)
(setq magit-completing-read-function 'ivy-completing-read)
(setq magit-commit-signoff t)
(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(global-set-key (kbd "<f3>") 'magit-status)
(global-set-key (kbd "C-x p") 'magit-find-file-completing-read)

(setq magit-repository-directories
      '(("~/DEV/work/git/tripleo/UPSTREAM" . 2)
        ("~/DEV/work/git/tripleo/OOOQ/" . 2)
        ("~/DEV/work/git/laptop_config/" . 2)
        ("~/DEV/work/git/ansible/" . 2)))

(add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0))) ; Disable it
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(setq git-commit-summary-max-length 80)
(add-to-list 'git-commit-known-pseudo-headers "Co-Authored-By")
(add-to-list 'git-commit-known-pseudo-headers "Related-Bug")
(add-to-list 'git-commit-known-pseudo-headers "Resolves")
(add-to-list 'git-commit-known-pseudo-headers "Closes-Bug")
(add-to-list 'git-commit-known-pseudo-headers "Implements")
(add-to-list 'git-commit-known-pseudo-headers "Change-Id")
(add-to-list 'git-commit-known-pseudo-headers "Depends-On")
(add-to-list 'git-commit-known-pseudo-headers "Needed-By")

;; (spacemacs/toggle-highlight-long-lines-globally-on)
(global-spacemacs-whitespace-cleanup-mode t)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'python-docstring-mode)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(custom-set-variables
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3"))

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

(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
(add-hook 'yaml-mode-hook #'ansible-doc-mode)

(define-key evil-normal-state-map (kbd "Q") (kbd "gqip"))
(setq sentence-end-double-space t)

(setq projectile-completion-system 'ivy)
(setq python-shell-interpreter "/usr/bin/python3.7")
(setq python-shell-interpreter-args "")

(setq ibuffer-modified-char ?✍)
(setq ibuffer-read-only-char ?✗)
(setq ibuffer-marked-char ?✓)
(setq ibuffer-deletion-char ?␡)
(setq ibuffer-show-empty-filter-groups nil)

(display-time-mode 1)
(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(setq display-time-world-list
      '(("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/Paris" "Paris")
        ("Pacific/Auckland" "Auckland")
        ("Asia/Shanghai" "Shanghai")))

(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")
(global-hi-lock-mode 1)
(global-git-gutter+-mode 1)
(global-color-identifiers-mode 1)
(windmove-default-keybindings)    ; Move between frames with Shift+arrow
(setq mouse-yank-at-point t)

(setq-default
  ad-redefinition-action 'accept                   ; Silence warnings for redefinition
  cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
  display-time-default-load-average nil            ; Don't display load average
  fill-column 80                                   ; Set width for automatic line breaks
  help-window-select t                             ; Focus new help windows when opened
  inhibit-startup-screen t                         ; Disable start-up screen
  ;; initial-scratch-message ""                       ; Empty the initial *scratch* buffer
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
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode 1)                           ; Hightlight current line
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent
(indent-guide-global-mode)
(xterm-mouse-mode -1)

(load "~/.spacemacs.d/mutt.el")
(load "~/.spacemacs.d/muttrc-mode.el")
(add-to-list 'auto-mode-alist '(".*neomutt.*" . mutt-mode))
(add-hook 'mutt-mode-hook 'smyx)
; wrap email body
(add-hook 'mutt-mode-hook 'turn-on-auto-fill)
(add-hook 'mutt-mode-hook 'turn-on-filladapt-mode)
(add-hook 'mutt-mode-hook 'flyspell-mode)

(setq mail-header-separator "")
(setq user-mail-address "gchamoul@redhat.com")
(setq user-full-name "Gaël Chamoulaud")

(message "strider's emacs have successful finished initialization!")
(message "------------------------------------------------------")
