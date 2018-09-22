;;; ys-lsp.el --- Language Server Protocol support

;;; Commentary:

;;; Code:

;; Basic LSP support
;; Language modules will add their own lsp setup if this is available
(use-package lsp-mode)

(with-eval-after-load "company"
  (use-package company-lsp
    :after lsp-mode
    :config
    (push 'company-lsp company-backends)))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-fine-references)))

(provide 'ys-lsp)
;;; ys-lsp.el ends here
