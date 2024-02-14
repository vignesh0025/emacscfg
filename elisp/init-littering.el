; This is very essential
(use-package no-littering
  :ensure t
  :config
	(setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  )

(provide 'init-littering)
