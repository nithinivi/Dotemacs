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


;;General packages
;;============================================================================


(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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
;;============================================================================


(use-package powerline
  :disabled
  :ensure t
  :config
  (setq powerline-default-separator 'utf-8))


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


;; python  configrations
;;============================================================================

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(remove-hook 'elpy-modules '(elpy-module-flymake highlight-indentation-mode))
(define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)

(use-package iedit
  :ensure t
  :bind ( "C-'" . iedit-mode))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-mode 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  )

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



;; Genrated content
;;============================================================================


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(package-selected-packages
   (quote
    (projectile emmet-mode web-mode git-timemachine git-gutter magit smartparens yasnippet-snippets exec-path-from-shell iedit elpy undo-tree counsel ivy-hydra ivy smex use-package-chords color-theme-sanityinc-solarized ag use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
