
(menu-bar-mode -1)


(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (company-tng-configure-default)

  )

(add-hook 'after-init-hook 'global-company-mode)
