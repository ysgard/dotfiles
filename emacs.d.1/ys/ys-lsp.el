;;; ys-lsp.el --- Language Server Protocol support

;;; Commentary:

;;; Code:

;; Basic LSP support
;; Language modules will add their own lsp setup if this is available
(use-package lsp-mode
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-fine-references)))

(provide 'ys-lsp)
;;; ys-lsp.el ends here
