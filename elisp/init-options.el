;; Font options

; Read from init-override.el if available
(defvar def-font-face)
(defvar def-font-height)
(set-face-attribute 'default nil :font def-font-face :height def-font-height)

; Set whitespace style
(setq whitespace-style '(tab-mark face tabs)) ; Show only Tab
(custom-set-faces '(whitespace-tab ((t (:foreground "black")))))
(add-hook 'prog-mode-hook 'whitespace-mode)
(setq-default tab-width 4)

;; Move Custom-Set-Variables to Different File
(setq custom-file (expand-file-name "custom-set-variables.el" user-emacs-directory))
(load custom-file 'noerror)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)

;; Don't scroll to middle of screen when cursor reaches end of frame
(setq scroll-conservatively 1000)

;; Cursor cover the actual space of a character
(setq x-stretch-cursor t)

;; Set frame-title
(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p) " ◉ %s" "  ●  %s - Emacs") project-name))))))

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Use "_" as part of word
(modify-syntax-entry ?_ "w")

;; Ask y/n instead of yes/no
(if (version<= emacs-version "28")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setopt use-short-answers t))

;; Scroll as per mouse wheel accurately
(pixel-scroll-precision-mode 1)

; Helpful Modes
(global-tab-line-mode)
(tab-bar-mode)
(menu-bar-mode)

; Hooks for prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'save-place-mode)

; Finer way of jumping word to word by detecting camelCase a
(add-hook 'prog-mode-hook 'global-subword-mode)

; Set indent tabs mode
(add-hook 'prog-mode-hook 'indent-tabs-mode)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

(provide 'init-options)
