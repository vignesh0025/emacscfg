;;; init-gc.el --- summary -*- lexical-binding: t -*-

;; Author: Vignesh D
;; Maintainer: Vignesh D
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentry:
;;; Taken from https://config.phundrak.com/emacs/basic-config.html#better-garbage-collection

;;; Code:
(setq gc-cons-threshold (* 512 1024 1024)) ; 512 MB
(setq garbage-collection-messages t)

(defmacro my/time (&rest body)
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun my/garbage-collect ()
  "Garbage collect and tell the user how much time it took."
  (message "Garbage collector ran for %.06fs"
           (my/time (garbage-collect))))

(defvar my/gc-timer nil "Timer for garbage collection.  See `my/garbage-collect-on-focus-lost'.")

(defun my/garbage-collect-on-focus-lost ()
  "Garbage collect when Emacs loses focus.

Garbage collection is only triggered 10 seconds after losing focus, and only once."
  (if (frame-focus-state)
      (when (timerp my/gc-timer)
       (cancel-timer my/gc-timer))
    (setq my/gc-timer (run-with-idle-timer 10 nil #'my/garbage-collect))))

;; Auto Garbage Collection
(add-function :after after-focus-change-function #'my/garbage-collect-on-focus-lost)

(provide 'init-gc)
;;; init-gc.el ends here
