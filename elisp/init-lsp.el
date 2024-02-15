(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watches t)
  (setq lsp-file-watch-threshold 5000)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package ccls
  :ensure t
  :config
  (if (bound-and-true-p vd-ccls-exec-d)
      (setq ccls-executable ""))
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda () (require 'ccls) (lsp))))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company
    :ensure t)

(provide 'init-lsp)

