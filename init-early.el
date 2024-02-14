(setq gc-cons-threshold 100000000)

(setq package-enable-at-startup nil)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq site-run-file nil)

(menu-bar-mode 1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10) ; Give some breathing space

(setq user-emacs-directory "~/.emacs.d")
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(provide 'early-init)
