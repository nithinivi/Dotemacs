(require 'package)
(require 'json)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(windmove-default-keybindings)
;;(delete-selection-mode t)
(column-number-mode t)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(set-cursor-color "black")


(setq column-number-mode 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(show-paren-mode t)

(bind-key  "M-+" 'text-scale-increase)
(bind-key  "M--" 'text-scale-decrease)


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))



(global-set-key (kbd "C-M-.") 'forward-sexp )
(global-set-key (kbd "C-M-,") 'backward-sexp)

(global-set-key (kbd "C-c d") 'kill-whole-line)


(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;;General packages
;;============================================================================


;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package ag
  :if (not noninteractive)
  :ensure ag)

(use-package color-theme-sanityinc-solarized
  :ensure t
  :config
  ;;(load-theme 'sanityinc-solarized-dark)
  )


(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1))

(use-package smex
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-initial-inputs-alist nil)
  )

(use-package ivy-hydra
  :ensure t)

(use-package counsel
  :ensure t
  :bind ("M-x" . counsel-M-x)
  :chords (("yy" . counsel-yank-pop))
  )

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package undo-tree
  :ensure t
  :chords (("uu" . undo-tree-visualize))
  :diminish undo-tree-mode:
  :config
  (global-undo-tree-mode 1)
  :bind ("C-}". undo-tree-undo)
  ("C-{" . undo-tree-redo))


;; visual
;;============================================================================


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


(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package counsel-projectile
  :ensure
  :config
  (counsel-projectile-mode)
  )

(use-package yasnippet-snippets
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package git-timemachine
  :ensure t)


(use-package dumb-jump   ;; for jumping to the source
  :ensure t
  :diminish dumb-jump-mode
  :bind (("C-." . dumb-jump-go)
         ("C-," . dumb-jump-back)
         ("C-M-g" . dumb-jump-quick-look)))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t) )

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))


(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (company-tng-configure-default)

  )

(add-hook 'after-init-hook 'global-company-mode)


;;exec
;;============================================================================
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;;golang
;;============================================================================

(use-package go-mode
  :ensure t)
(use-package go-eldoc
  :ensure t)
(use-package company-go
  :ensure t)
(setq gofmt-command "goimports")
;; UPDATE: gofmt-before-save is more convenient then having a command
;; for running gofmt manually. In practice, you want to
;; gofmt/goimports every time you save anyways.
(add-hook 'before-save-hook 'gofmt-before-save)

(global-set-key (kbd "C-;") 'company-complete)


(defun my-go-mode-hook ()
  ;; UPDATE: I commented the next line out because it isn't needed
  ;; with the gofmt-before-save hook above.
  ;; (local-set-key (kbd "C-c m") 'gofmt)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'yas-minor-mode)




;; python  configrations
;;============================================================================
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda"))
  :config
  ;; If you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; If you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; If you want auto-activation, include:
  (conda-env-autoactivate-mode t)
  ;; Activate the project/virtual env you want to use.
  ;; Via M-x conda-env-activate RET analyticd-pysystemtrade
  ;; or
  ;; (conda-env-activate "analyticd-pysystemtrade")
  )




(use-package elpy

  :ensure t
  :config
  (elpy-enable))


;; (remove-hook 'elpy-modules 'elpy-module-flymake)
;; (global-set-key (kbd "C-;") 'yas-expand )
;; (setq highlight-indentation-mode nil)

;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")
(define-key python-mode-map (kbd "C-c r") 'python-shell-send-region )



(use-package ein
  :ensure t)

(use-package iedit
  :ensure t
  :bind ( "C-c i" . iedit-mode))

(add-hook 'python-mode-hook 'yas-minor-mode)

(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))

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

;;dired-mode
;;============================================================================
(use-package dired-details
  :load-path "~/.emacs.d/package/dired-details"
  :config
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))
;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq dried-dwim-target t)

;; Genrated content
;;============================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(fci-rule-color "#073642")
 '(package-selected-packages
   (quote
    (exce-path-from-shell restclient-test dired-details docker conda ein nlinum drag-stuff aggressive-indent dumb-jump projectile emmet-mode web-mode git-timemachine git-gutter magit smartparens yasnippet-snippets exec-path-from-shell iedit elpy undo-tree counsel ivy-hydra ivy smex use-package-chords color-theme-sanityinc-solarized ag use-package)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
