;; PART 1
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq ring-bell-function 'ignore
      visible-bell nil)


(tool-bar-mode -1)             ; Hide the outdated icons
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI


(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(windmove-default-keybindings)


(add-to-list 'default-frame-alist '(font . "Input-19" ))
(set-face-attribute 'default t :font "Input-18" )
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

(use-package emacs
  :init
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message "")))


(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

(use-package emacs
  :init
	(when (eq system-type 'darwin)
		(setq mac-command-modifier 'meta
		      mac-option-modifier 'none
          mac-option-key-is-meta nil
		      mac-command-key-is-meta t
          
          )))


(setq custom-safe-themes t)
;; (use-package kanagawa-theme
;;   :demand
;;   :config
;;   (load-theme 'kanagawa))

(use-package emacs
  :init
  (defun ab/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers -1))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons)


;; PART 2

(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after .5s instead of 1s
  :config
  (which-key-mode))

(use-package counsel)
(use-package swiper)

(use-package general
  :demand
  :config

  (general-create-definer leader-keys
    :keymaps 'override-global-map
    :prefix "M-c")

  (general-define-key "M-x" 'counsel-M-x)
  (general-define-key "C-x C-f" 'counsel-find-file)
  (general-define-key "C-s" 'swiper-isearch)
  (general-define-key "M-y" 'counsel-yank-pop)
  (general-define-key "C-x b" 'ivy-switch-buffer)
  
  

  (leader-keys
    "x" '(execute-extended-command :which-key "execute command")

    ;; Emacs
    "e" '(:ignore t :which-key "emacs")
    "e e" '((lambda () (interactive)
              (find-file user-init-file))
            :which-key "open init file")
    
    "e r" '(restart-emacs :which-key "restart emacs")
    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t) 
    "bd"  'kill-current-buffer

    "w" '(:ignore t :which-key "Window")
    "wl"  'windmove-right
    "wh"  'windmove-left
    "wk"  'windmove-up
    "wj"  'windmove-down
    "w/"  'split-window-right
    "w-"  'split-window-below
    "wx"  'delete-window
    )
  )


(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  :bind
  ("C-}". undo-tree-undo)
  ("C-{" . undo-tree-redo)
  ("C-x u" . undo-tree-visualize)

  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode 1)
  (unbind-key "M-_" undo-tree-map)
  (unbind-key "C-_" undo-tree-map)
  )


(use-package yasnippet-snippets)         ; Collection of snippets



(use-package yasnippet                  ; Snippets
  :config
  (setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (yas-global-mode 1)
  )

(use-package projectile
	:demand
  :general
  (leader-keys
    :states 'normal
    "SPC" '(projectile-find-file :which-key "find file")

    ;; Buffers
    "b b" '(projectile-switch-to-buffer :which-key "switch buffer")

    ;; Projects
    "p" '(:ignore t :which-key "projects")
    "p <escape>" '(keyboard-escape-quit :which-key t)
		"p p" '(projectile-switch-project :which-key "switch project")
		"p a" '(projectile-add-known-project :which-key "add project")
	  "p r" '(projectile-remove-known-project :which-key "remove project")
    )
	:init
	(projectile-mode +1))

(use-package ivy
  :init
  (ivy-mode))

(use-package magit
  :general
  (leader-keys
    "g" '(:ignore t :which-key "git")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log :which-key "log"))
  (general-nmap
   "<escape>" #'transient-quit-one))


(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package vterm)

(use-package vterm-toggle
  :general
  (leader-keys
    "'" '(vterm-toggle :which-key "terminal")))

(use-package emacs
  :init
 	(global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;; PART 3

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package emacs
	:init
	(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
	(add-to-list 'default-frame-alist '(ns-appearance . light))
	(setq ns-use-proxy-icon  nil)
	(setq frame-title-format nil))


(use-package emacs
  :config
  (setq backup-directory-alist `(("." . "~/.saves"))))

(use-package company-mode
  :init
  (global-company-mode))

;; eglot
(use-package emacs
  :hook (zig-mode . eglot-ensure)
  :hook (rust-mode . eglot-ensure)
  :hook (go-mode . eglot-ensure)
  :hook (typescript-mode . eglot-ensure)
  :hook (c-mode . eglot-ensure)
  :hook (c++-mode . eglot-ensure)
  :general
  (leader-keys
    "l" '(:ignore t :which-key "lsp")
    "l <escape>" '(keyboard-escape-quit :which-key t)
    "l r" '(eglot-rename :which-key "rename")
    "l a" '(eglot-code-actions :which-key "code actions"))
  :config
  (setq eglot-autoreconnect t
        eglot-autoshutdown t)

  )

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package zig-mode
  :general
  (leader-keys
    "m" '(:ignore t :which-key "mode")
    "m <escape>" '(keyboard-escape-quit :which-key t)
    "m b" '(zig-compile :which-key "build")
    "m r" '(zig-run :which-key "run")
    "m t" '(zig-test :which-key "test")))

(use-package rust-mode
  :general
  (leader-keys
    "m" '(:ignore t :which-key "mode")
    "m <escape>" '(keyboard-escape-quit :which-key t)
    "m b" '(rust-compile :which-key "build")
    "m r" '(rust-run :which-key "run")
    "m t" '(rust-test :which-key "test")
    "m k" '(rust-check :which-key "check")
    "m c" '(rust-run-clippy :which-key "clippy")))

(use-package go-mode)
(use-package gotest
  :general
  (leader-keys
    "m" '(:ignore t :which-key "mode")
    "m <escape>" '(keyboard-escape-quit :which-key t)
    "m t" '(go-test-current-project :which-key "test")
    "m r" '(go-run :which-key "run")))
(use-package typescript-mode)

(use-package rg
  :general
  (leader-keys
    "f" '(rg-menu :which-key "find")))

(use-package emacs
  :init
  (setq-default fill-column 80)
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#edf0f0"
                      :background "transparent")
  (global-display-fill-column-indicator-mode 1))
(use-package org-mode)
(use-package org-tree-slide
  :custom
  (org-image-actual-width nil)
  :config
  (setq org-tree-slide-heading-emphasis t      ;; Emphasize headings
        org-tree-slide-slide-in-effect t       ;; Enable slide-in effect
        org-tree-slide-activate-message "Presentation mode ON"
        org-tree-slide-deactivate-message "Presentation mode OFF"
        org-tree-slide-modeline-display 'both  ;; Show slide number & mode
        org-tree-slide-skip-done nil           ;; Skip DONE headlines
        org-tree-slide-skip-comments t         ;; Skip commented sections
        org-tree-slide-content-margin 10)       ;; Add margin for readability
  (org-tree-slide-simple-profile))


;;(use-package ox-qmd)



