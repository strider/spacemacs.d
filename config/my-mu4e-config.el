;; My mu4e configuration:

(setq mu4e-maildir (expand-file-name "~/Mail/redhat-gmail"))

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
(setq mu4e-enable-mode-line t)

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
(setq message-send-mail-function 'smtpmail-send-it)
;; (setq sendmail-program "/usr/bin/msmtp")
(setq smtpmail-smtp-server "smtp.corp.redhat.com")
(setq message-sendmail-extra-arguments '("--read-envelope-from"))

(setq mu4e-compose-format-flowed t)

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

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
(add-hook 'mu4e-view-mode-hook #'visual-line-mode)
(global-set-key (kbd "<f5>") 'mu4e-update-index)
(global-set-key (kbd "<f7>") 'mu4e-jump-to-list)
