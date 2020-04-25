(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq package-check-signature nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep all backup and auto-save files in one directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)
(column-number-mode t)
(setq column-number-mode 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")



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
;; (set-face-attribute 'default nil :height 155)

(setq dired-dwim-target t)

(defun cleanup-buffer-safe ()
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;;General shortcuts
;;============================================================================


(bind-key  "M-+" 'text-scale-increase)
(bind-key  "M--" 'text-scale-decrease)


(global-set-key (kbd "C-M-.") 'forward-sexp )
(global-set-key (kbd "C-M-,") 'backward-sexp)

(global-set-key (kbd "C-c d") 'kill-whole-line)


;;(global-set-key (kbd "C-w") 'kill-word)
(global-set-key [f5] 'call-last-kbd-macro)


(setq next-line-add-newlines t)

;;Generel
;;============================================================================

;; (use-package  aweshell
;;   :ensure t )

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package ag
  :if (not noninteractive)
  :ensure ag)


;;(highlight-indentation-mode -1)

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
  :bind
  ("M-x" . counsel-M-x)
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))


(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package undo-tree
  :ensure t
  :chords (("iii" . undo-tree-visualize))
  :diminish undo-tree-mode:
  :config
  (global-undo-tree-mode 1)
  :bind ("C-}". undo-tree-undo)
  ("C-{" . undo-tree-redo))


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


;;https://joaotavora.github.io/yasnippet/snippet-expansion.html
(use-package yasnippet-snippets
  :ensure t
  :config
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/elpa/yasnippet-snippets-0.17/snippets/"
                             ;;,"~/.emacs.d/elpa/yasnippet-snippets-0.16/snippets"
                             ))

    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "S-SPC") yas-maybe-expand)
    ;; Bind `C-c y' to `yas-expand' ONLY.
    (define-key yas-minor-mode-map (kbd "M-]") #'yas-expand)
    ( yas-global-mode 1)
    (setq yas-snippet-dirs (append yas-snippet-dirs
                                   '(yasnippet-snippets-dir)))

    )
  )


(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;; visual
;;============================================================================

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
;; autocomplete  configrations
;;============================================================================

;; (use-package company-fuzzy
;;   :load-path  "~/.emacs.d/package/company-fuzzy"
;;   )
(use-package company-jedi
  :ensure t)

(use-package company
  :ensure t
  :bind ("C-M-;" . company-complete)
  :init (global-company-mode t)
  :config

  (setq company-idle-delay .1)
  (setq company-minimum-prefix-length 3)
  (setq company-quickhelp-delay 0)
  (setq company-selection-wrap-around t)
  ;;(company-quickhelp-mode 1)
  (company-tng-configure-default)
  ;;(global-company-fuzzy-mode 1)
  (add-to-list 'company-backends 'company-jedi)
  )

(add-hook 'after-init-hook 'global-company-mode)
;; scratch buffer  configrations
;;============================================================================

(use-package persistent-scratch
  :ensure t
  :config

  (persistent-scratch-autosave-mode 1))

;; python  configrations
;;============================================================================


;; python
(setq python-shell-interpreter "python3")


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


(defun python-buffer-mode()
  (interactive)
  (progn( yas-minor-mode 1)
        (highlight-indentation-mode))
  )


(use-package elpy

  :ensure t
  :config
  (elpy-enable)
  (add-hook 'python-mode-hook 'python-buffer-mode)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  )


(remove-hook 'elpy-modules 'elpy-module-flymake)
;; (global-set-key (kbd "C-;") 'yas-expand )
;; (setq highlight-indentation-mode nil)
;; (setq python-shell-interpreter "ipython")
;; ;;python-shell-interpreter-args "-i--simple-prompt")


(defun my-python-mode-before-save-mode-hook()
  (interactive)
  (when (eq major-mode 'python-mode)
    (progn (cleanup-buffer-safe)
           (elpy-format-code))))


(define-key python-mode-map (kbd "C-c r") 'python-shell-send-region )



;; (use-package ein
;;   :ensure t)

(use-package iedit
  :ensure t
  :bind ( "C-c i" . iedit-mode))



(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))

(use-package conda
  :ensure t
  :config
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t))




;; (delete-selection-mode t)
(require 'server)
(unless (server-running-p)
  (server-start))

                                        ; shell configrations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





                                        ; comment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when window-system
  (progn
    (load "~/.emacs.d/init_gui.el")
    (load "~/.emacs.d/init_projects.el")))

(when (not window-system)
  (load "~/.emacs.d/init_terminal.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4555bf2b98f0ffef52bc4870f3014304b0e2ed22549c395dffc10af88d577791" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   (quote
    (terminal-here mmm-mode ox-publish vmd-mode ox-gfm imenus imenu-anywhere material-theme go-autocomplete yasnippet-snippets yaml-mode xref-js2 which-key web-mode use-package-chords smex smartparens smart-shift sexy-monochrome-theme restclient persistent-scratch org-bullets nlinum neotree minions lsp-ui kubernetes ivy-hydra indium iedit goto-chg gorepl-mode golden-ratio go-playground go-guru go-complete git-timemachine git-gutter flycheck-yamllint exec-path-from-shell emmet-mode elpy dumb-jump dockerfile-mode docker cython-mode counsel-projectile conda company-tern company-lsp company-jedi color-theme-sanityinc-solarized cherry-blossom-theme aggressive-indent ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
