;;; ys-yaml.el --- yaml configuration

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(provide 'ys-yaml)
;;; ys-yaml.el ends here
