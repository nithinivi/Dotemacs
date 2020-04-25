(windmove-default-keybindings)
(set-face-attribute 'default nil :height 150)

(setq exec-path-from-shell-check-startup-files -1)
(load-theme 'manoj-dark)
(use-package color-theme-sanityinc-solarized
  :ensure t)


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


;;modeline programming
;;============================================================================


(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))


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

(add-to-list 'exec-path "~/go_code/bin")


;; ;;golang
;; ;;============================================================================

(use-package company-go
  :load-path "/home/nithin/.emacs.d/packages/company-go"
  )

;; (defvar go-tab-width 4
;;   "Set the `tab-width' in Go mode. Default is 8.")

;; (defvar go-use-gometalinter nil
;;   "Use gometalinter if the variable has non-nil value.")

(defun my-go-run-main ()
  (interactive)
  (shell-command
   (format "go run %s"     (shell-quote-argument (buffer-file-name)))
   ))


(defun spacemacs//go-enable-gometalinter ()
  "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
  (setq flycheck-disabled-checkers '(
                                     go-vet
                                     go-build
                                     go-test
                                     go-errcheck)))

(defun my-go-var-shortcut ()
  (interactive)
  (insert " := ")
  )

(defun my-go-write-into-ch-shortcut ()
  (interactive)
  (insert "<-")
  )


(use-package go-mode
  :ensure t
  :init (progn
          (autoload 'go-mode "go-mode" nil t)
          (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
          )
  :config (progn
            (if  (string-match "make" compile-command)
                (set (make-local-variable 'compile-command)
                     "go build -v && go test -v && go vet"))
            (if (executable-find "goimports")
                (setq gofmt-command "goimports")
              (setq gofmt-args '("-local" "liftoff/")))
            )

  :bind (:map go-mode-map
              ("C-<" . my-go-write-into-ch-shortcut)
              ("C-;" . my-go-var-shortcut)
              ("M-." . godef-jump)
              ("M-*" . pop-tag-mark)
              ("C-c RET" . compile)
              ("<C-return>" . my-go-run-main))
  )

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode 1)
            (flymake-mode -1)
            (auto-complete-mode 1)
            (spacemacs//go-enable-gometalinter)
            ) )

(add-hook 'before-save-hook 'gofmt-before-save)
;; (add-hook 'go-mode-hook 'flycheck-mode)

(use-package flycheck-gometalinter
  :defer t
  )

(use-package go-complete
  :ensure t
  :hook
  (completion-at-point-functions . go-complete-at-point)
  )

(use-package go-guru
  :ensure t
  :hook
  (go-mode-hook . #'go-guru-hl-identifier-mode)
  )



(use-package gorepl-mode
  :ensure t
  :hook
  (go-mode-hook . #'gorepl-mode)
  )

(use-package go-playground
  :ensure t
  )



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
;; (use-package dired-details
;;   :load-path "~/.emacs.d/package/dired-details/"
;;   :config
;;   (setq-default dired-details-hidden-string "--- ")
;;   (dired-details-install))
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
        )
      )

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
  (sql-connect connection)
  )


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

;; when opening a org file, don't collapse headings
(setq org-startup-folded nil)

;; wrap long lines. don't let it disappear to the right
(setq org-startup-truncated nil)

;; when in a url link, enter key should open it
(setq org-return-follows-link t)

;; make org-mode” syntax color embedded source code
(setq org-src-fontify-natively t)

;;

;; (use-package ox-beamer
;;   :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))


;; (use-package cython
;;   :ensure  t)

;; markdown mode
;;============================================================================
;;(setq markdown-command "multimarkdown"))
;;(setq markdown-command "pandoc")
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

(use-package company-tern
  :ensure t)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                                                   (tern-mode)
                                                   (company-mode)))

(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)
;;============================================================================



(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))

;; relative nliunm mode
;;============================================================================

(use-package nlinum
  :ensure t
  :bind ("M-[" . nlinum-mode)
  )


;; (use-package nlinum-relative
;;   :ensure t
;;   :config
;;   ;; something else you want
;;   (nlinum-relative-setup-evil)

;;   (setq nlinum-relative-redisplay-delay 0)      ;; delay
;;   (setq nlinum-relative-current-symbol "->")      ;; or "" for display current line number
;;   (setq nlinum-relative-offset 0)
;;   (add-hook 'prog-mode-hook 'nlinum-relative-mode))




;; yaml support
;;============================================================================

(use-package highlight-indentation
  :load-path "/home/nithin/.emacs.d/packges/Highlight-Indentation-for-Emacs/highlight-indentation.el"
  :config
  (setq highlight-indentation t)
  )


(use-package smart-shift
  :ensure t)


(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)


(use-package yaml-mode
  :ensure t
  :config
  (setq yaml-indent-offset 2))
;; :hook (yaml-mode . smart-shift-mode-hook)) ;


(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))


(use-package golden-ratio
  :ensure t
  )




;; lsp  configrations
;;============================================================================
(use-package which-key
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)

(use-package company-lsp :ensure t :commands company-lsp)
(use-package company-lsp :commands company-lsp)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package flycheck
  :ensure t
  :hook (python-mode-hook . flycheck-mode)
  )

(add-hook 'go-mode-hook #'lsp-deferred)
;; terminal mode setup
;;============================================================================

(use-package terminal-here
  :ensure t
  )

(global-set-key (kbd "C-<f6>") #'terminal-here-launch)
(global-set-key (kbd "C-<f7>") #'terminal-here-project-launch)



;; Writing book  configrations
;;============================================================================

;; (use-package vmd-mode
;;   :ensure t
;;   :defer t)

(use-package ox-gfm
  :ensure t)

;; (add-hook 'markdown-mode-hook 'vmd-mode)

(use-package cl-lib
  :ensure t)

(use-package mmm-mode
  :commands mmm-mode
  :init (add-hook 'markdown-mode-hook  (lambda () (mmm-mode 1)))
  :config
  (progn

    (mmm-add-classes '((markdown-lisp
                        :submode lisp-mode
                        :front "^```lisp[\n\r]+"
                        :back "^```$")))

    (mmm-add-classes '((markdown-ini
                        :submode conf-unix-mode
                        :face mmm-declaration-submode-face
                        :front "^```ini[\n\r]+"
                        :back "^```$")))
    (mmm-add-classes '((markdown-python
                        :submode python-mode
                        :face mmm-declaration-submode-face
                        :front "^```python[\n\r]+"
                        :back "^```$")))
    ))
