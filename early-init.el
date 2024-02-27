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

(if (not (require 'init-override nil t)) (message "no override"))

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

; NOTE: We can only disable the modes in early-init.el
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 0)
(blink-cursor-mode 1)
(scroll-bar-mode 0)

(setq package-enable-at-startup nil
	  inhibit-startup-message t
	  site-run-file nil
	  package-native-compile t
	  read-process-output-max (* 2048 1024)
	  frame-resize-pixelwise t
	  )

(setq user-emacs-directory "~/.emacs.d")
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Load def fonts from init-override.el
;; They must be kept here since we need to update default-frame-alist
(defvar def-font-face (if (boundp 'vd-font-family) vd-font-family "Hack"))
(defvar def-font-sz (if (boundp 'vd-font-sz) vd-font-sz 11))
(defvar def-font-height (* def-font-sz 10))
(defvar font-face-size (format "%s-%d" def-font-face def-font-sz))

(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)

; backquote(`) is required below to add-to-list. Then only comma(,) will replace font-face-size with its value
(add-to-list 'default-frame-alist `(font . ,font-face-size))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(alpha-background . 0.9))

(require 'init-gc)

(provide 'early-init)
