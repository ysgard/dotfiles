;; ys-base.el --- Basic editor configuration

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

;; Unbind C-l so we can use it as a prefix key
(global-unset-key "\C-l")

;; Keybinds for commonly used operations
(global-set-key (kbd "C-l l") 'linum-mode) ; Turn line numbers on/off
(global-set-key (kbd "C-l w") 'whitespace-mode) ; toggle whitespace chars
(global-set-key (kbd "C-l b") 'buffer-menu)
(global-set-key (kbd "C-l n") 'ys/new-empty-buffer)
(global-set-key (kbd "C-l k") 'kill-this-buffer)
(global-set-key (kbd "C-l K") 'kill-buffer-and-window)
(global-set-key (kbd "C-l <tab>") 'indent-buffer)
(global-set-key (kbd "C-`") 'eshell)
(global-set-key (kbd "C-l /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-l r") 'align-regexp)

;; Shortcut for query-replace-regexp
(defalias 'qrr 'query-replace-regexp)

;; Always indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Better keymaps for isearch
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-S") 'isearch-backward-regexp)

;; Increase/decrease font
(global-set-key (kbd "C-l =") 'text-scale-increase)
(global-set-key (kbd "C-l -") 'text-scale-decrease)

;; New frame
(global-set-key (kbd "C-l N") 'make-frame)

;; Window navigation
(global-set-key (kbd "C-l o") 'other-window)

;; Buffer nav
(global-set-key (kbd "C-M-<right>") 'ys/next-emacs-buffer)
(global-set-key (kbd "C-M-<left>") 'ys/previous-emacs-buffer)
(global-set-key (kbd "M-<right>") 'ys/next-user-buffer)
(global-set-key (kbd "M-<left>") 'ys/previous-user-buffer)
(global-set-key (kbd "M-<down>") 'scroll-up-command)
(global-set-key (kbd "M-<up>") 'scroll-down-command)




(provide 'ys-base)
;;; ys-base ends here
