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

(setq gc-cons-threshold 100000000)

(setq package-enable-at-startup nil)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq site-run-file nil)

(menu-bar-mode)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10) ; Give some breathing space

(message user-emacs-directory)

(setq user-emacs-directory "~/.emacs.d")
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;;(if (bound-and-true-p menu-bar-mode)
;;   (message "menu-bar-mode is on")
;;  (message "menu-bar-mode is off"))

(provide 'early-init)
