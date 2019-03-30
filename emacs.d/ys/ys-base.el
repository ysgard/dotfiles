;; ys-base.el --- Basic editor configuration

;;; Commentary:
;;;
;;; Non-package, non-display settings go in here

;;; Code:

;; Always ask for y/n instead of 'yes/no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Sane defaults for marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;; We prefer UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)
(load-library "iso-transl")
(require 'iso-transl)

;; Turn off backups.  We store everything in git anyhow.
(setq make-backup-files nil)

;; Deal with temporary files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Show all options when running apropos
(setq apropos-do-all t)

;; If a file changed on disk, prompt us to refresh
(global-auto-revert-mode t)

;; Interpret color codes in shells
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Load $PATH
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
 (exec-path-from-shell-initialize))

(provide 'ys-base)
;;; ys-base ends here
