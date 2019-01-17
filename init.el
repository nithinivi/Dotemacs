(require 'package)

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
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(windmove-default-keybindings)
(delete-selection-mode t)
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


(use-package ag
  :if (not noninteractive)
  :ensure ag)

(use-package color-theme-sanityinc-solarized
  :ensure t
  :config
  (load-theme 'sanityinc-solarized-dark))


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

(use-package powerline
  :disabled
  :ensure t
  :config
  (setq powerline-default-separator 'utf-8))
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




;;  programming


(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  :hook prog-mode
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  :hook 'prog-mode)

(use-package rainbow-mode
  :ensure t
  :config
  (setq rainbow-x-colors nil)
  :hook prog-mode)

(use-package aggressive-indent
  :ensure t
  :hook prog-mode )

(add-hook 'prog-mode-hook 'electric-pair-mode)



;; porject management

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(setq projectile-completion-system 'ivy)

(use-package fzf
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package dumb-jump
  :ensure t
  :diminish dumb-jump-mode
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

;; git
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

(use-package company
  :ensure t
  :diminish
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-idle-delay t)
  (setq company-minimum-prefix-length 2)

  (setq company-dabbrev-downcase nil)
  )

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1 )
  (global-set-key (kbd "M-/") 'company-yasnippet))


(use-package yasnippet-snippets
  :ensure t)

(use-package company
  :ensure t)
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends '(company-anaconda
				   :with company-capf )))

;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable)
;;   (setq elpy-company-add-completion-from-shell t))


;; (use-package    feebleline
;;   :load-path "~/.emacs.d/extras/feebleline"
;;   :custom       (feebleline-show-git-branch             t)
;;   (feebleline-show-dir                    t)
;;   (feebleline-show-time                   nil)
;;   (feebleline-show-previous-buffer        nil)
;;   :config       (feebleline-mode 1))

(use-package  virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(package-selected-packages
   (quote
    (company-anaconda virtualenvwrapper anaconda-mode yasnippet-snippets yasnippet company git-timemachine git-gutter magit dumb-jump exec-path-from-shell fzf projectile aggressive-indent rainbow-mode rainbow-delimiters smartparens undo-tree counsel ivy-hydra ivy smex use-package-chords color-theme-sanityinc-solarized ag use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
