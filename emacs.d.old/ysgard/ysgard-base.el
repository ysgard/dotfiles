;; ~/.emacs.d/base.el --- Summary

;;; Commentary:
;;;
;;; Non-package, non-display settings go in here

;;; Code:

;; Tabs and indents
(setq-default tab-width 4
              indent-tabs-mode nil) ; Spaces instead of tabs

;; This should probably belong in another file, but
;; as it relies on no externals... meh
;; Set tab and style for C code
(setq-default c-basic-offset 4 c-default-style "bsd")

;; Always ask for y/n instead of 'yes/no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Sane defaults for marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)
      
;; Set default text behaviour
(setq-default word-wrap t)
; (global-visual-line-mode t) ; Same, but breaks long lines into semantic units
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Function to turn off linum easily for particular modes
(defun nolinum ()
  "Turn on/off line numbers."
  (interactive)
  (message "Deactivated linum mode")
  (global-linum-mode 0)
  (linum-mode 0))

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

(provide 'ysgard-base)
;;; ysgard-base ends here
