;;; ys-narrows.el --- narrowing/completion frameworks

;;; Commentary:

;; I normally use ido/flex/smex for this kind of stuff (ys-ido),
;; so this file represents an alternative using counsel/ivy/swiper

;;; Code:

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy)
  (setq ivy-virtual-abbreviate 'full)
  :after (magit)
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind (("C-l C-r" . ivy-resume)
         ("C-j" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-l j" . ivy-immediate-done))
  :hook (view-mode . (lambda ()
                       (define-key view-mode-map
                         (kbd "C-j") 'ivy-switch-buffer))))

(use-package counsel
  :bind (("C-c i m" . counsel-imenu)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-j" . ivy-switch-buffer)
         ("M-y" . counsel-yank-pop)
         ("M-l" . counsel-bookmark)))

(defun ys/swiper-recenter (&rest args)
  "Recenter display after swiper"
  (recenter))

;; Swiper replaces helm
(use-package swiper
  :config
  (advice-add 'swiper :after #'ys/swiper-recenter)
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(provide 'ys-narrows)
;;; ys-narrows.el ends here
