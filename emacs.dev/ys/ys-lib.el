;;; ys-lib.el --- Functions and macros for general use

;;; Commentary:

;;; This script contains common functions used to simplify other scripts, or
;;; condense common functionality

;; system-types:
;;   gnu/linux     - linux
;;   darwin        - os x
;;   berkeley-unix - FreeBSD
;;   ms-dos        - MS-DOS
;;   windows-nt    - Win32

;;; Code:

(require 'ys-package)

;; New standard library
;; f.el    - files and paths  https://github.com/rejeep/f.el
;; s.el    - strings          https://github.com/magnars/s.el
;; dash.el - lists            https://github.com/magnars/dash.el
(use-package f)
(use-package s)
(use-package dash)

(defmacro ys/with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun ys/new-empty-buffer ()
  "Open a new, empty buffer (based on Xah's)."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

;; Functions to cycle easily through user buffers while skipping annoying *emacs* buffers
(defun ys/next-user-buffer ()
  "Next buffer that doesn't start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer))))

(defun ys/previous-user-buffer ()
  "Switch to previous user buffer."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer))))

(defun ys/next-emacs-buffer ()
  "Switch to next emacs '*' buffer."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer))))

(defun ys/previous-emacs-buffer ()
  "Switch to previous emacs '*' buffer."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer))))

(defun ys/setenv-if-not-set (env-var val)
  "Set the environment variable ENV-VAR to VAL if it is not set."
  (interactive "senvvar: \nsvalue: ")
  (if (not (getenv env-var))
      (setenv env-var val)))

(defun ys/toggle-comment-on-line ()
  "Comment/uncomment current line."
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun ys/die-tabs ()
  "Use 2 spaces for tabs, dammit!"
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))
  
(defun ys/eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file.  The eshell is renamed to match that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun ys/eshell-x ()
  "Exits an eshell and closes that window."
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;; Function to turn off linum easily for particular modes
(defun ys/nolinum ()
  "Turn on/off line numbers."
  (interactive)
  (message "Deactivated linum mode")
  (global-linum-mode 0)
  (linum-mode 0))

(provide 'ys-lib)
;;; ys-lib.el ends here
