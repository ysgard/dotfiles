;;; ys-dired.el --- Dired config

;;; Commentary:

;; Used for file/directory manipulations.  Absurdly powerful.

;;; Code:

;; Keep dired buffers updated when the file system changes
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(require 'dired)
(define-key evil-motion-state-map (kbd "SPC d") 'dired-jump)
;; Reuse dired buffers and avoid unnecessary proliferation
(use-package dired-single
  :config
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer))

(use-package dired-icon
  :hook ((dired-mode . dired-icon-mode)))

;; A function for deleting the file being edited
;; A bit dangerous, so not bound to any key.
;; Run it with M-x delete-current-buffer-file
(defun delete-current-buffer-file ()
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

;; And a function for renaming the file being edited, bound to C-x C-r.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
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
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(provide 'ys-dired)
;;; ys-dired.el ends here
