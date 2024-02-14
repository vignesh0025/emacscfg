; Font options
(setq default-frame-list '(
			   (font . "Hack-15")
			   (fullscreen . maximized)
			   ))

(set-face-attribute 'default nil :font "Hack" :height 110)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    ))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'save-place-mode)

(setq whitespace-style '(tab-mark)) ; Show only Tab
(add-hook 'prog-mode-hook 'whitespace-mode)

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


(provide 'init-options)
