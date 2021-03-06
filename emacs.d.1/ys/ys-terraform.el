;;; ys-terraform.el --- Terraform configuration

;;; Commentary:

;;; Code:

(use-package hcl-mode
  :config
  (custom-set-variables '(hcl-indent-level 2))
  :mode "\\.hcl\\'")

(use-package terraform-mode
  :config
  (custom-set-variables '(terraform-indent-level 2))
  :mode "\\.tf\\'")

(provide 'ys-terraform)
;;; ys-terraform.el ends here
