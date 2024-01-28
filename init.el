(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10) ; Give some breathing space

(set-face-attribute 'default nil :font "Hack Nerd Font" :height 110)

;; I mindlessly press ESC, so stop me from wreaking havoc
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; C-x z (OR) M-x repeat RET -> repeats last operation
;; M-g g (OR) M-g M-g -> goto line:

;; minibuffer-electric-default-mode -> Hides default argument in minibuffer, if we write some entry to it
;; delete-trailing-whitespace
;; delete-empty-blanklines

;; C-x C-x -> highlight last selected region (same as "gv" in vim)
;; When region is selected: C-x C-c -> moves to start/end of slection

(setq user-emacs-directory "~/.emacs.d")
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Install package manager that actually installs the packages
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
      (goto-char (point-max)):
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; This is very essential
(straight-use-package 'no-littering)

(setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;; This installs the use-package to manage the plugins and configuration
(straight-use-package 'use-package)
(straight-use-package 'el-patch)

(use-package org
  :straight t
  :ensure t
  )

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
  :straight t
  :demand t ;; Autoload on start. Don't do lazyload
  :init
  (setq evil-want-integration t) ;; TODO: Check if this are still applicable
  (setq evil-want-keybinding nil) ;; TODO: Check if this are still applicable
  :hook (evil-mode . vd/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;Exit to normal mode using C-g (EMACS World)
  )

;; EVIL mode integration i.e. keybindings for other modes such as calender
(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

;; M-x all-the-icons-install-fonts is required to actually install the icons
(use-package all-the-icons
  :straight t
  :ensure t
  )

(use-package doom-modeline
  :straight t
  :ensure t ;; Make sure its installed if its not installed
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes t)
  )

(use-package rainbow-delimiters
  :straight t
  :hook  (prog-mode . rainbow-delimiters-mode)
  )

;; Try IVY !!!!
(use-package helm
  :straight t
  :init
  (setq completion-styles '(flex))
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  )

(use-package helm-rg
  :straight t
  :ensure t
  )

(use-package projectile
  :straight t
  :demand t
  :config
  (projectile-mode +1)
  )

(use-package helm-projectile
  :straight t
  :demand t
  :config
  (helm-projectile-on)
  )

(use-package magit
  :straight t
  :demand t
  )

(use-package lsp-mode
  :straight t
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
	 (lambda () (require 'ccls) (lsp))))

(use-package anti-zenburn-theme
  :straight t
  :config
  (load-theme `anti-zenburn t) ; t is required to trust this theme without saying we need to trust this
  )

(use-package doom-themes
  :straight t
  :ensure t
  )



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "c7737b9fc3471779c8e51ea0a37834d24aa80a0d6a79b215e7501227ada39855" default))
 '(helm-minibuffer-history-key "M-p"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Keymap related things in one place
(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  )

(use-package general
  :straight t
  :ensure t
  :config
  (general-evil-setup t)

  (general-create-definer vd/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  )

(use-package hydra
  :straight t
  :ensure t
  )

(defhydra hydra-zoom (:timeout 3 global-map "<f2>")
  "Zoom"
  ("l" text-scale-increase "in")
  ("h" text-scale-decrease "out")
  )

(defhydra hydra-buffer ()
  "Buffer Navigate"
  ("l" next-buffer "Next")
  ("h" previous-buffer "Previous")
  )

(defhydra hydra-error ()
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")
    )

(defvar whitespace-mode nil)
(defvar display-line-numbers-mode nil)
(defhydra hydra-toggle (:color red)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_n_ display-line-numbers-mode %`display-line-numbers-mode
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode

"
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("n" display-line-numbers-mode nil)
  ("q" nil "quit")
  )

;; The below two are another level man!!
(vd/leader-keys
  "t"  '(:ignore t :which-key "Theme")
  "tt" '(load-theme :which-key "choose theme")

  "SPC" '(switch-to-buffer :which-key "Go to buffer")

  "p" '(:ignore t :which-key "Projectile")
  "pm" '(projectile-command-map :which-key "Projectile-Commands")
  "pp"  '(helm-projectile-find-file :which-key "Find File Recursive")
  "pf"  '(helm-find-files :which-key "Find File")

  "h" '(:ignore t :which-key "Hydra Operations")
  "ht" '(hydra-toggle/body :which-key "Toggle operations")
  "hz" '(hydra-zoom/body :which-key "Zoom")
  "hb" '(hydra-buffer/body :which-key "Navigate Buffer")
  "he" '(hydra-error/body :which-key "Errors")
  )
