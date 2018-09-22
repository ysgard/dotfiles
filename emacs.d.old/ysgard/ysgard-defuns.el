;;; ~/.emacs.d/utils.el --- Summary:

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

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun ysgard-new-empty-buffer ()
  "Open a new, empty buffer (based on Xah's)."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

;; Functions to cycle easily through user buffers while skipping annoying *emacs* buffers
(defun ysgard-next-user-buffer ()
  "Next buffer that doesn't start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer))))

(defun ysgard-previous-user-buffer ()
  "Switch to previous user buffer."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer))))

(defun ysgard-next-emacs-buffer ()
  "Switch to next emacs '*' buffer."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer))))

(defun ysgard-previous-emacs-buffer ()
  "Switch to previous emacs '*' buffer."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer))))

(defun ysgard-setenv-if-not-set (env-var val)
  "Set the environment variable ENV-VAR to VAL if it is not set."
  (interactive "senvvar: \nsvalue: ")
  (if (not (getenv env-var))
      (setenv env-var val)))

(defun ysgard-toggle-comment-on-line ()
  "Comment/uncomment current line."
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun ysgard-die-tabs ()
  "Use 2 spaces for tabs, dammit!"
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))
  
(defun set-exec-path-from-shell-PATH ()
  "Set up Emac's `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell 
			(replace-regexp-in-string "[ \t\n]*$" ""
									  (shell-command-to-string
										"$SHELL --login -i -c 'echo $PATH'"))))
	(setenv "PATH" path-from-shell)
	(setq exec-path (split-string path-from-shell path-separator))))

(defun eshell-here ()
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

(defun eshell/x ()
  "Exits an eshell and closes that window."
  (insert "exit")
  (eshell-send-input)
  (delete-window))



(provide 'ysgard-defuns)
;;; ysgard-defuns.el ends here
