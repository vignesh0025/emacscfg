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

(setq whitespace-style '(tab-mark)) ; Show only Tab
(add-hook 'prog-mode-hook 'whitespace-mode)

(provide 'init-options)
