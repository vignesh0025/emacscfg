
(add-hook 'c-ts-mode-hook
		  (lambda ()
		  (message "setting c indent mode")
		  (setq c-ts-mode-indent-offset 4)
		  (setq c-ts-mode-indent-style "c-vd-style")
		  ))

(defun vd/treesit-config ()
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (cmake "https://github.com/uyha/tree-sitter-cmake")
	  (c "https://github.com/tree-sitter/tree-sitter-c")
	  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
	  ))

  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

)

(if (treesit-available-p)
    (vd/treesit-config))

;; (treesit-language-available-p 'c) ; returns if c grammer is available and can be loaded

(use-package verilog-ts-mode
  :ensure t)

(require 'init-modemap)

(provide 'init-treesit)
