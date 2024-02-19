;; The following will be always have Emacs mode by default
(defun vd/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circa-server-mode
		  circa-chat-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; use C-Z to toggle evil and emacs mode
(use-package evil
  :ensure t
  :demand t ;; Autoload on start. Don't do lazyload
  :init
  (setq evil-want-integration t) ;; TODO: Check if this are still applicable
  (setq evil-want-keybinding nil) ;; TODO: Check if this are still applicable
  :hook (evil-mode . vd/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;Exit to normal mode using C-g (EMACS World)
  (setq evil-symbol-word-search t) ;When using * and # consider _ as part of word
  )

;; EVIL mode integration i.e. keybindings for other modes such as calender
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(provide 'init-evil)
