;;; package --- Summary
;;; ~/.emacs.d/lisp/settings-other.el

;;; Commentary:

;;; Package settings that are too small or trivial to move to their own file
;;; Packages should not rely on other packages, as this file is loaded
;;; first in the package settings load order

;;; Code:

;; Turn on company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; Dired+
(require 'dired)
(when (require 'dired+ nil 'noerror)
  (progn
    ;; Reuse dired buffers and avoid unnecessary proliferation
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (toggle-diredp-find-file-reuse-dir 1)))

;; Dockerfiles
(when (require 'dockerfile-mode nil 'noerror)
  (progn
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
    (add-to-list 'auto-mode-alist '("\\.docker\\'" . dockerfile-mode))))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ido
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq magit-completing-read-function 'magit-ido-completing-read)
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(ido-vertical-mode 1)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Turn on key-chording
(when (require 'key-chord nil 'noerror)
  (key-chord-mode 1))

;; Mode icons
(when (require 'mode-icons nil 'noerror)
  (mode-icons-mode))

;; Org-mode
;; Bullets
(require 'org)
(when (require 'org-bullets nil 'noerror)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; Source syntax highlighting
(setq org-src-fontify-natively t)
;; Load org to markdown export library
(require 'ox-md nil t)
(require 'ox-reveal nil t)

;; Rainbow delimiters
(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Smart mode line
(when (require 'smart-mode-line nil 'noerror)
  (progn
    (setq sml/vc-mode-show-backend t)
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'dark)
    (sml/setup)
    (sml/apply-theme 'powerline)))

;; Terraform
(when (require 'terraform-mode nil 'noerror)
  '(terraform-indent-level 2))

;; Tramp
(setq tramp-default-method "ssh")

;; yaml-mode
(when (require 'yaml-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
  
(provide 'ysgard-misc)
;;; ysgard-misc ends here
