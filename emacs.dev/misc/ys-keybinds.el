;;; ~/.emacs.d/keybinds.el --- Summary

;;; Keybindings for various packages and default emacs,
;;; gathered in one place

;;; Commentary:

;;; Code:

;; Non-external (base) keybinds

;; Use C-l insead of super for machines where the windows key is
;; intercepted at a low level (I'm looking at you, Windows)
;; Normally bound to recenter (which I never use) it's easy to type.
(global-unset-key "\C-l")


;; DISPLAY
;;;;;;;;;;;;

;; Turn line numbers on/off
(global-set-key (kbd "C-l l") 'linum-mode)

;; Increase/decrease text size
(global-set-key (kbd "C-l =") 'text-scale-increase)
(global-set-key (kbd "C-l -") 'text-scale-decrease)

;; Toggle whitespace mode
(global-set-key (kbd "C-l w") 'whitespace-mode)

;; new frame
(global-set-key (kbd "C-l N") 'make-frame)

;; NAVIGATION
;;;;;;;;;;;;;;;

;; Move between windows using alt-left and alt-right
(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-l o") 'other-window)

;; Buffer nav
(global-set-key (kbd "C-M-<right>") 'ysgard-next-emacs-buffer)
(global-set-key (kbd "C-M-<left>") 'ysgard-previous-emacs-buffer)
(global-set-key (kbd "M-<right>") 'ysgard-next-user-buffer)
(global-set-key (kbd "M-<left>") 'ysgard-previous-user-buffer)

;; Scroll up/Scroll down
(global-set-key (kbd "M-<down>") 'scroll-up-command)
(global-set-key (kbd "M-<up>") 'scroll-down-command)

;; Dired
(global-set-key (kbd "C-l d") 'dired-jump)

;; BUFFERS
;;;;;;;;;;;;
(global-set-key (kbd "C-l b") 'buffer-menu)

;; New buffer
(global-set-key (kbd "C-l n") 'ysgard-new-empty-buffer)

;; Kill buffer
(global-set-key (kbd "C-l k") 'kill-this-buffer)
(global-set-key (kbd "C-l K") 'kill-buffer-and-window)

;; We always want to indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Make the tragically inaccessible keys for isearch-*-regexp more accessible
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)

;; Indent current buffer
(global-set-key (kbd "C-l <tab>") 'indent-buffer)

; MISC
;;;;;;;;;;;;;

;; Ansi-term
(global-set-key (kbd "C-`") 'ansi-term)

;; Define shortcut for query & replace
(defalias 'qrr 'query-replace-regexp)

;; Toggle comments
(global-set-key (kbd "C-l /") 'comment-or-uncomment-region)

;; Align-regexp
(global-set-key (kbd "C-l r") 'align-regexp)


;; EXTERNAL
;;;;;;;;;;;;;

;; Ido & Smex
(when (require 'smex nil 'noerror)
  (progn
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    ;; Old M-x
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

;; Magit
(when (require 'magit nil 'noerror)
  (progn
    (global-set-key (kbd "C-l g") 'magit-status)))

;; Multiterm
(when (require 'multi-term nil 'noerror)
  (global-set-key (kbd "C-l `") 'multi-term-dedicated-open))

;; Treemacs
(when (require 'treemacs nil 'noerror)
  (progn
    (global-set-key (kbd "C-l x") 'treemacs)))
;;; k/j should move line up/down in treemacs
(add-hook 'treemacs-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "k") #'treemacs-previous-line)
              (local-set-key (kbd "j") #'treemacs-next-line)
              (local-set-key (kbd "/") #'isearch-forward-regexp))))

(provide 'ysgard-keybinds)
;;; ysgard-keybinds.el ends here
