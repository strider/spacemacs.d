;; My Org Configuration

(setq org-roam-directory "~/org-roam")
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User key bindings
;;
;; org-journal user keybinding
;; - create a new journal entry
(spacemacs/set-leader-keys "oj" 'org-journal-new-entry)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `org-indent-mode' does not play nice with git-gutter, so let's disable
;; it :-(
(setq git-gutter:disabled-modes '(org-mode))

(setq calendar-week-start-day 1)	; Weeks start on monday
(setq calendar-date-style 'european)
(setq european-calendar-style t)

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?B)
(setq org-default-priority ?A)

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* ((variable-tuple (cond ((x-list-fonts "Agave")            '(:font "Agave"))
                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
        (base-font-color     (face-foreground 'default nil 'default))
        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                            (?B . (:foreground "LightSteelBlue"))
                            (?C . (:foreground "OliveDrab"))))

(with-eval-after-load 'org
  (setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "DOING(s!)"
                  "BLOCKED(b@)"
                  "REVIEW(r!/!)"
                  "|"
                  "DONE(d!)"
                  "ARCHIVED(a@)"
                  "CANCELED(c@)"))))

(with-eval-after-load 'org
  (setq org-todo-keyword-faces
      '(("TODO" . "SlateGray")
        ("DOING" . "DarkOrchid")
        ("BLOCKED" . "Firebrick")
        ("REVIEW" . "Teal")
        ("DONE" . "ForestGreen")
        ("CANCELED" . (:foreground "red" :weight bold :strike-through t))
        ("ARCHIVED" . "SlateBlue"))))

(setq org-clock-persist-query-save t)
(setq org-clock-in-switch-to-state "DOING")
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-src-preserve-indentation t)
(setq org-enforce-todo-dependencies t)
(setq org-link-abbrev-alist
      '(("colissimo" . "http://www.coliposte.net/particulier/suivi_particulier.jsp?colispart=")
        ("launchpad" . "https://bugs.launchpad.net/bugs/")
        ("review"    . "https://review.opendev.org/#/c/")
        ("rhbz"      . "https://bugzilla.redhat.com/show_bug.cgi?id=")
        ("JIRA"      . "https://projects.engineering.redhat.com/browse/")))

(with-eval-after-load 'org
  (setq org-capture-templates
        '(("x" "TODO PERSONAL Tasks" entry (file+headline "~/org/inbox.org" "PERSONAL TASKS")
           "\n* TODO [#A] %\\1 - %\\2%?\n:PROPERTIES:\n:DESCRIPTION: %^{DESCRIPTION}\n:TITLE: %^{TITLE}\n:END:\n- Added: %u\n" :prepend t :kill-buffer t)
          ("w" "TODO WORK Tasks" entry (file+headline "~/org/inbox.org" "WORK TASKS")
           "\n* TODO [#A] %\\1 - %\\2%?\n:PROPERTIES:\n:DESCRIPTION: %^{DESCRIPTION}\n:TITLE: %^{TITLE}\n:END:\n- Added: %u\n" :prepend t :kill-buffer t)
          ))

  (setq org-agenda-files '("~/org/inbox.org"
                           "~/org/misc.org"
                           "~/org/technical_notes.org"
                           "~/org/todo.org"))
  (add-to-list 'org-agenda-files org-journal-dir)
  (setq org-default-notes-file "~/org/inbox.org")
  )

(message "end of my-org-config")
