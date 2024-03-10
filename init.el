;; delete-trailing-whitespace
;; delete-empty-blanklines

;; file-name-handler-alist-original comes from early-init.el
(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq file-name-handler-alist file-name-handler-alist-original)
			(makunbound 'file-name-handler-alist-original)
			))

(require 'init-options)

(require 'init-elpaca)

(require 'init-littering)

(use-package el-patch
  :ensure t)

(require 'init-evil)

(use-package org
  :ensure t)

;; M-x all-the-icons-install-fonts is required to actually install the icons
(use-package all-the-icons
  :ensure t
  )

(use-package doom-modeline
  :ensure t ;; Make sure its installed if its not installed
  :init
  (doom-modeline-mode 1)
  )

;; (use-package spaceline
;;   :ensure t ;; Make sure its installed if its not installed
;;   :init
;;   (spaceline-emacs-theme)
;;   (spaceline-toggle-minor-modes-off)
;;   )


(use-package rainbow-delimiters
  :ensure t
  :hook  (prog-mode . rainbow-delimiters-mode)
  )

(use-package ivy
  :ensure t
  :config
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
(ivy-mode)
)

(use-package ivy-hydra
  :after ivy)

(use-package ivy-rich
  :after ivy
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode)
  )

(use-package ivy-xref
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  )

;; (use-package ivy-prescient
;;   :after ivy
;;   :init
;;   (setq ivy-prescient-retain-classic-highlighting t)
;;   :config
;;   (ivy-prescient-mode)
;;   )

(use-package swiper)

(use-package counsel
  :ensure t
  :config
  (counsel-mode)
  )

(use-package projectile
  :ensure t
  :demand t
  :config
  (setq projectile-git-fd-args "-0 -E .git -tf --strip-cwd-prefix -c never")
  (add-to-list 'projectile-globally-ignored-directories '"^\\.ccls-cache$")
  (projectile-mode +1)
  )

(use-package counsel-projectile
  )

;; ;; Try IVY !!!!
;; (use-package helm
;;   :ensure t
;;   :init
;;   (setq completion-styles '(flex))
;;   :config
;;   (helm-mode 1)
;;   (global-set-key (kbd "M-x") #'helm-M-x)
;;   (global-set-key (kbd "C-x C-f") #'helm-find-files)
;;   )

;; (use-package helm-rg
;;   :ensure t
;;   )

;; (use-package helm-projectile
;;   :ensure t
;;   :demand t
;;   :config
;;   (helm-projectile-on)
;;   )

;; (use-package helm-themes
;;   :ensure t)

;; (use-package helm-xref
;;   :ensure t)

;; (use-package swiper-helm
;;   :ensure t)

(require 'init-git)

(require 'init-treesit)

;; Delete lsp-mode packages to make sure below thing is taken into effect 1st time
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)
;; lsp-doctor can be used to check the lsp functions
(require 'init-lsp)

(use-package spacemacs-theme
  :ensure t
  :config
  ;; (load-theme `spacemacs-light t)
  ;; (load-theme `spacemacs-dark t)
  )

(use-package  color-theme-modern
  :ensure t
  :config
  ;; (load-theme `rotor t)
  ;; (load-theme `gray30 t)
  ;; (load-theme `jedit-grey t)
  )

(use-package doom-themes
  :config
  (load-theme `doom-palenight t)
  )

(use-package nyan-mode
  :config
  (nyan-mode)
  )

(use-package dts-mode
  :config
  )

;; Keymap related things in one place
(use-package which-key
  :defer t
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  )

(use-package general
  :ensure t
  :init
  (general-auto-unbind-keys)
  :config
  (general-evil-setup t)

  (general-create-definer vd/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  )

(use-package hydra
  :defer t
  :ensure t
  )

;; Disabled due to https://github.com/Wilfred/helpful/issues/329
;; (use-package helpful
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-h f") #'helpful-callable)
;;   (global-set-key (kbd "C-h v") #'helpful-variable)
;;   (global-set-key (kbd "C-h k") #'helpful-key)
;;   (global-set-key (kbd "C-h x") #'helpful-command)
;;   (global-set-key (kbd "C-h o") #'helpful-at-point)
;;   )
;;
(elpaca-wait)


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

; (defvar whitespace-mode nil)
; (defvar display-line-numbers-mode nil)
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

  ; "SPC" '(switch-to-buffer :which-key "Go to buffer")
  "SPC" '(switch-to-buffer :which-key "Go to buffer")
  "b" '(switch-to-buffer :which-key "Go to buffer")

  "p" '(:ignore t :which-key "Projectile")
  "pm" '(projectile-command-map :which-key "Projectile-Commands")
  "pp"  '(counsel-projectile-find-file :which-key "Find File Recursive")
  "pf"  '(helm-find-files :which-key "Find File")

  "h" '(:ignore t :which-key "Hydra Operations")
  "ht" '(hydra-toggle/body :which-key "Toggle operations")
  "hz" '(hydra-zoom/body :which-key "Zoom")
  "hb" '(hydra-buffer/body :which-key "Navigate Buffer")
  "he" '(hydra-error/body :which-key "Errors")

  "g" '(:ignore t :which-key "Git Operations")
  "gi" '(git-gutter:statistic :which-key "GitGutter Statistics")
  "gr" '(git-gutter:revert-hunk :which-key "GitGutter Revert Change")
  "gd" '(git-gutter:popup-hunk :which-key "GitGutter Popup Change")
  "ga" '(git-gutter:stage-hunk :which-key "GitGutter Stage Change")
  "gs" '(magit-status :which-key "Git Status")
  "gl" '(magit-log :which-key "Show log")
  "gb" '(magit-blame :which-key "Blame File")

  "l"  '(:ignore t :which-key "LSP Operations")
  "lr" '(lsp-workspace-restart :which-key  "Restart LSP")
  "lh" '(lsp-describe-session :which-key "LSP Describe Session")
  "ll" '(counsel-imenu :which-key "Code Outline")
  "la" '(lsp-execute-code-action :which-key "Code Action")

  "e"  '(:ignore t :which-key "Flycheck Operations")
  "el" '(flycheck-list-errors :which-key "List errors")
  "es" '(flycheck-verify-setup :which-key "Verify current enabled/supported error checker")
  "ec" '(flycheck-verify-checker :which-key "Verify checker working")
  "ed" '(flycheck-describe-checker :which-key "Describe the checker")

  "s"  '(swiper :which-key "Swiper")
  )


;; I mindlessly press ESC, so stop me from wreaking havoc
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file)
(define-key evil-normal-state-map (kbd "M-p") 'switch-to-buffer)

(define-key evil-normal-state-map (kbd "] g") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "[ g") 'git-gutter:previous-hunk)
(define-key evil-normal-state-map (kbd "[ f") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "] f") 'flycheck-next-error)

