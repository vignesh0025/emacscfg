;;; package --- Summary:
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watches t)
  (setq lsp-file-watch-threshold 5000)
  (setq lsp-modeline-code-actions-mode t)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  )

(use-package ccls
  :disabled
  :ensure t
  :config
  (if (bound-and-true-p vd-ccls-exec)
      (setq ccls-executable vd-ccls-exec)
    (message "WARNING: ccls-executable -not-set"))
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode cuda-mode) .
	 (lambda () (require 'ccls) (lsp))))

(use-package flycheck
  :ensure t
  :config
  ;; This tells the flycheck to inherit whatever the variable load-path us having
  ;; Avoid "cannot load open file error"
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package company
    :after lsp-mode
    :ensure t
	:config
	(add-to-list 'company-backends '(company-yasnippet company-capf))
	(setq company-minimum-prefix-length 1
		  company-idle-delay 0.0) ;; default is 0.2
	(add-hook 'after-init-hook 'global-company-mode)
	(setq company-tooltip-align-annotations t)
	)

; Provides icons for Company-Mode popup like LSP
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :defer t
  :ensure t
  :config
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)

;; TODO: yatemplate and auto-insert-mode
;; https://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html
;; https://github.com/mineo/yatemplate

(provide 'init-lsp)
;;; init-lsp.el ends here

