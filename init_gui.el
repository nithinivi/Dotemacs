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




(use-package go-mode
  :ensure t
  :config
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  )

;;




(use-package gorepl-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook #'gorepl-mode))

(use-package go-playground
  :ensure t
  )

;;lsp-support
(add-hook 'go-mode-hook 'lsp-deferred)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator)))
  )

(when window-system (set-exec-path-from-shell-PATH))
(setenv "GOPATH" "/home/nithin/go")



(add-to-list 'exec-path "/home/nithin/go/bin")
(add-hook 'before-save-hook 'gofmt-before-save)



(defun auto-complete-for-go ()
  (auto-complete-mode 1)
  (flymake-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1)
  )
(add-hook 'go-mode-hook 'auto-complete-for-go)


;; (use-package go-autocomplete
;;   :ensure t go-autocomplete)

;; (with-eval-after-load 'go-mode
;;   (require 'go-autocomplete))


;; build

;; go imports

(defun my-go-var-shortcut ()
  (interactive)
  (insert " := "))

(defun my-go-mode-hook ()
                                        ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
                                        ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
                                        ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "C-;") 'my-go-var-shortcut)
  (local-set-key (kbd "<C-return>") 'compile)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)


;; company mode support

(use-package company-go
  :ensure t)

(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)))



;;web-mode
;;============================================================================

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

  (setq web-mode-content-types-alist
        '(("json" . "/some/path/.*\\.api\\'")
          ("xml"  . "/other/path/.*\\.api\\'")
          ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))

  )
(use-package emmet-mode
  :ensure t
  :hook (html-mode . emmet-mode))

(use-package restclient
  :ensure t)

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




;; lsp  configrations
;;============================================================================


(use-package lsp-mode
  :ensure t
  :hook (python-mode . lsp)
  :commands lsp)

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package flycheck
  :ensure t
  :hook (python-mode-hook . flycheck-mode)
  )

(add-hook 'go-mode-hook #'lsp-deferred)


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
(progn
  ;; org-mode setup

  ;; when opening a org file, don't collapse headings
  (setq org-startup-folded nil)

  ;; wrap long lines. don't let it disappear to the right
  (setq org-startup-truncated nil)

  ;; when in a url link, enter key should open it
  (setq org-return-follows-link t)

  ;; make org-mode” syntax color embedded source code
  (setq org-src-fontify-natively t)

  ;;
  )


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; markdown mode
;;============================================================================


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; JS mode
;;============================================================================


(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  ())



(use-package xref-js2
  :ensure t)

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

  (define-key js-mode-map (kbd "M-.") nil)

  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))



(autoload 'js2-mode "js2-mode" nil t)
(autoload 'js2-jsx-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-jsx-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(custom-set-variables '(js2-strict-inconsistent-return-warning nil))
(custom-set-variables '(js2-strict-missing-semi-warning nil))

(use-package indium
  :ensure t)
;;============================================================================
