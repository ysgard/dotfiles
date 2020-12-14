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
(require 'cl)

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

;; Despite the names, these were all taken from Ohai Emacs.  Credit goes
;; to Bodil Stokke for these.

(defun ys/exec (command)
  "Run a shell COMMAND and return its output as a string, whitespace-trimmed."
  (s-trim (shell-command-to-string command)))

(defun ys/exec-with-rc (command &rest args)
  "Run a shell COMMAND and return a list containing two values: its return code and its whitespace trimmed output."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun ys/is-exec (command)
  "Return true if COMMAND is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun ys/resolve-exec (command)
  "If COMMAND is an executable on the system search path, return its absolute path, or nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun ys/exec-if-exec (command args)
  "If COMMAND satisfies `ys/is-exec', run it with ARGS and return its output as per `ys/exec', or nil."
  (when (ys/is-exec command) (ys/exec (s-concat command " " args))))


(defun ys/eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(require 'ansi-color)
(defun  display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun ys-online? ()
  "Returns (up) if we have a network connection, nil otherwise"
  (if (and (functionp 'network-interface-list)
	   (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
			      (member 'up (first (last (network-interface-info
							(car iface)))))))
	    (network-interface-list))
    t))

;; Rename the file being edited
(defun ys/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting"
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun ys/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; Create a nice compilation window
(defun ys-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))




(provide 'ys-lib)
;;; ys-lib.el ends here
