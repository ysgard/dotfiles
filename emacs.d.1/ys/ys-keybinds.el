;; ys-keybinds.el --- Basic editor configuration

;;; Commentary:

;;; Code:

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

;; Extensions
(global-set-key (kbd "C-l g") 'magit-status)
(global-set-key (kbd "C-l d") 'dired-jump)
(global-set-key (kbd "C-l x") 'treemacs)
(global-set-key (kbd "C-l r") 'rustic-popup)
;; Terminals

(bind-key "C-!" 'ys/eshell-here)


(provide 'ys-keybinds)
;;; ys-base ends here
