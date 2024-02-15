(setq inhibit-startup-message t)


;; C-x z (OR) M-x repeat RET -> repeats last operation
;; M-g g (OR) M-g M-g -> goto line:

;; minibuffer-electric-default-mode -> Hides default argument in minibuffer, if we write some entry to it
;; delete-trailing-whitespace
;; delete-empty-blanklines

;; C-x C-x -> highlight last selected region (same as "gv" in vim)
;; When region is selected: C-x C-c -> moves to start/end of slection
(defvar better-gc-cons-threshold 134217728); 128mb

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

;; Auto Garbage Collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;; Setup paths to load files
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path user-emacs-directory)
(update-to-load-path (expand-file-name "elisp" user-emacs-directory))

(if (require 'init-override nil t)
    (message "overriding...")
  (message "no override"))

(require 'init-options)

(require 'init-modemap)

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
  (setq doom-modeline-minor-modes t)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook  (prog-mode . rainbow-delimiters-mode)
  )

;; Try IVY !!!!
(use-package helm
  :ensure t
  :init
  (setq completion-styles '(flex))
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  )

(use-package helm-rg
  :ensure t
  )

(use-package projectile
  :ensure t
  :demand t
  :config
  (projectile-mode +1)
  )

(use-package helm-projectile
  :ensure t
  :demand t
  :config
  (helm-projectile-on)
  )

(require 'init-git)

(require 'init-treesit)

(require 'init-lsp)

(use-package anti-zenburn-theme
  :ensure t
  :disabled
  :config
  (load-theme `anti-zenburn t) ; t is required to trust this theme without saying we need to trust this
  )

(use-package doom-themes
  :disabled
  :ensure t)

(use-package spacemacs-theme
  :ensure t
  :config
  (load-theme `spacemacs-light t)
  (load-theme `spacemacs-dark t)
 )

;; Keymap related things in one place
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  )

(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (general-create-definer vd/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  )

(use-package hydra
  :ensure t
  )

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


;; I mindlessly press ESC, so stop me from wreaking havoc
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
