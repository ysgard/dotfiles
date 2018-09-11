;;; ys-c.el --- C/C++ config

;;; Commentary:

;;; Code:

(use-package irony-eldoc)

(use-package irony
  :defer t
  :bind (:map irony-mode-map
              ([remap completion-at-point] . irony-completion-at-point-async)
              ([remap complete-symbol] . irony-completion-at-point-async))
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cbd-autosetup-compile-options)
         (irony-mode . irony-eldoc)))

(use-package company-irony
  :after (company)
  :defer t
  :config
  (push company-backends 'company-irony)
  :bind (:map c-mode-map
              ("<tab>" . company-complete)))

(use-package company-c-headers
  :after (company)
  :defer t
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include")
  (push company-backends 'company-c-headers))

(use-package flycheck-irony
  :after (company flycheck)
  :config
  (require 'company-gtags)
  (push company-backends 'company-gtags)
  :hook (flycheck-mode . flycheck-irony-setup))

(add-hook 'c-mode-hook (lambda ()
                         (flycheck-mode)
                         (company-mode)))

(provide 'ys-c)
;;; ys-c.el ends here
