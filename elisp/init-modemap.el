(add-to-list 'auto-mode-alist '("\\.testsuite\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.test_suite\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.haaf\\'" . conf-mode))

(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-ts-mode))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; (treesit-language-available-p 'cmake)
;; (treesit-language-available-p 'c)

(provide 'init-modemap)
