(windmove-default-keybindings)
(set-face-attribute 'default nil :height 125

                    )
(setq exec-path-from-shell-check-startup-files -1)

(use-package color-theme-sanityinc-solarized
  :ensure t
  :config
  ;;(load-theme 'deeper-blue)
  )

(use-package color-theme-sanityinc-solarized
  :load-path "~/.emacs.d/package/hemisu-theme/"
  ;;(load-theme 'hemisu-light-theme)
  )



(use-package powerline
  :disabled
  :ensure t
  :config
  (setq powerline-default-separator 'utf-8))



(use-package hideshow
  :hook ((prog-mode . hs-minor-mode)))

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))

(global-set-key (kbd "C-=") 'hs-toggle-hiding)

;;genral programming
;;============================================================================



(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package git-timemachine
  :ensure t)



(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;;exec
;;============================================================================
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "GOROOT" "GOBIN"))
  (exec-path-from-shell-initialize))

;; ;;golang
;; ;;============================================================================

;; (use-package go-mode
;;   :ensure t)
;; (use-package go-eldoc
;;   :ensure t)
;; (use-package company-go
;;   :ensure t)
;; (setq gofmt-command "goimports")
;; ;; UPDATE: gofmt-before-save is more convenient then having a command
;; ;; for running gofmt manually. In practice, you want to
;; ;; gofmt/goimports every time you save anyways.
;; (add-hook 'before-save-hook 'gofmt-before-save)

;; (global-set-key (kbd "?\t") 'company-complete)


;; (defun my-go-mode-hook ()
;;   ;; UPDATE: I commented the next line out because it isn't needed
;;   ;; with the gofmt-before-save hook above.
;;   ;; (local-set-key (kbd "C-c m") 'gofmt)
;;   (local-set-key (kbd "M-.") 'godef-jump))
;; (add-hook 'go-mode-hook (lambda ()
;;                           (set (make-local-variable 'company-backends) '(company-go))
;;                           (company-mode)))

;; (add-hook 'go-mode-hook 'my-go-mode-hook)
;; (add-hook 'go-mode-hook 'go-eldoc-setup)
;; (add-hook 'go-mode-hook 'company-mode)
;; (add-hook 'go-mode-hook 'yas-minor-mode)





;;web-mode
;;============================================================================

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  )
(use-package emmet-mode
  :ensure t
  )

;; (use-package restclient
;;   :load-path "~/.emacs.d/package/restclient")

;;dired-mode
;;===========================================D=================================
(use-package dired-details
  :load-path "~/.emacs.d/package"
  :config
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))
;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq dried-dwim-target t)



;;sql specific
;;============================================================================
(setq sql-connection-alist
      '((server1 (sql-product 'postgres)
                 (sql-port 5432)
                 (sql-server "localhost")
                 (sql-user "nithin")
                 (sql-database "kyc"))
        ))

(defun my-sql-connect (product connection)
  ;; load the password
  (require my-password "my-password.el.gpg")

  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection my-sql-password)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (sql-connect connection))


;; Projectile mode
;;============================================================================



;;slack mode
;;============================================================================
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil

  )

;;org mode
;;============================================================================

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; markdown mode
;;============================================================================


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
