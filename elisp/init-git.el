(use-package magit
  :ensure t
  :demand t
  )

(use-package git-gutter
  :ensure t
  :demand t
  :config
  (global-git-gutter-mode +1)
  )

(provide 'init-git)
