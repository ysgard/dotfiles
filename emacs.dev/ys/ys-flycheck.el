;;; ys-flycheck.el --- Spot errors on the fly.

;;; Commentary:


;;; Code:

;; Bind M-n and M-p to navigate to next/previous errors
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; Install and configure
(use-package flycheck
  :config
  ;; Start is automatically for all modes except elisp
  ;; elisp linter is just stupid. (bind to free symbol, anyone?)
  (add-hook 'find-file-hook
            (lambda ()
              (when (not (equal 'emacs-lisp-mode major-mode))
                (flycheck-mode)))))

;; Turn the modeline red when flycheck spots errors
(use-package flycheck-color-mode-line
  :config
  (with-eval-after-load "flycheck"
    (setq flycheck-highlighting-mode 'symbols)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))


(with-eval-after-load "flycheck"
  (set-face-background 'flycheck-error "#660000")
  (set-face-foreground 'flycheck-error nil)
  (set-face-background 'flycheck-warning "#331800")
  (set-face-foreground 'flycheck-warning nil)
  (require 'flycheck-color-mode-line)
  (set-face-background 'flycheck-color-mode-line-error-face "#440000")
  (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
  (set-face-background 'flycheck-color-mode-line-info-face nil)
  (set-face-foreground 'flycheck-color-mode-line-error-face "#ffffff")
  (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffffff")
  (set-face-foreground 'flycheck-color-mode-line-info-face nil))

(with-eval-after-load "helm"
  (use-package helm-flycheck
    :bind (("C-c ! !" . helm-flycheck))))

(provide 'ys-flycheck)
;;; ys-flycheck.el ends here

