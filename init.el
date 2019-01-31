(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)



;; Base  Package  Configrations
;;=====================================================================================

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    projectile
    magit
    use-package
    elpy
    flycheck
    material-theme
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)



;; Basic Customizations
;;=================================================================================

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defalias 'yes-or-no-p 'y-or-n-p)
(load-theme 'material t)
;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(windmove-default-keybindings)
(delete-selection-mode t)
(column-number-mode t)
(menu-bar-mode 1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(set-cursor-color "black")
(setq visible-bell nil)

(setq column-number-mode 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(show-paren-mode t)

(bind-key  "M-+" 'text-scale-increase)
(bind-key  "M--" 'text-scale-decrease)

;; Package customiztion
;;====================================================================================

(use-package ag
  :if (not noninteractive)
  :ensure ag)

;; (use-package color-theme-sanityinc-solarized
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-solarized-light))


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
  :bind("C-x C-f" . counsel-find-file)
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

;; progrmming configrations
;;====================================================================================

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  :hook prog-mode
  )


(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(setq projectile-completion-system 'ivy)

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

;; (use-package company
;;   :ensure t
;;   :diminish
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode)

  ;; (setq company-idle-delay t)
  ;; (setq company-minimum-prefix-length 2)

  ;; (setq company-dabbrev-downcase nil)
  ;; )

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




;; python configrations
;;====================================================================================


;; python configrations
;;====================================================================================

(elpy-enable)
;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)



;; System Genrated
;;=====================================================================================


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (py-autopep8 material-theme flycheck elpy ein better-defaults yasnippet-snippets web-mode virtualenvwrapper use-package-chords undo-tree smex smartparens projectile magit jedi ivy-hydra iedit git-timemachine git-gutter fzf exec-path-from-shell emmet-mode dumb-jump counsel company-anaconda color-theme-sanityinc-solarized ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
