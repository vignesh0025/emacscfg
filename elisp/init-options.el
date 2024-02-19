; Font options

; Read from init-override.el if available
(setq def-font-face (if (boundp 'vd-font-family) vd-font-family "Hack"))
(setq def-font-sz (if (boundp 'vd-font-sz) vd-font-sz 11))
(setq def-font-height (* def-font-sz 10))

(setq font-face-size (format "%s-%d" def-font-face def-font-sz))

; (setq test-list nil)
; (add-to-list 'test-list `(font . ,font-face-size))

; backquote(`) is required below to add-to-list. Then only comma(,) will replace font-face-size with its value
(add-to-list 'default-frame-alist `(font . ,font-face-size))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil :font def-font-face :height def-font-height)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (add-hook 'window-setup-hook 'toggle-frame-maximized t)
	    ))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'save-place-mode)

(setq whitespace-style '(tab-mark face tabs)) ; Show only Tab
(custom-set-faces '(whitespace-tab ((t (:foreground "black")))))
(add-hook 'prog-mode-hook 'whitespace-mode)
(setq-default tab-width 4)
(setq-local c-ts-mode-intent-offset 4)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

(pixel-scroll-precision-mode 1)

;; Use "_" as part of word
(modify-syntax-entry ?_ "w")

(provide 'init-options)
