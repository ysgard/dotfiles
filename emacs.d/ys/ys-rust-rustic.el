;;; ys-rust-rls.el --- Summary

;;; Rust support for Emacs

;;; Commentary:

;;; Use rustic to provide rust integration with rls/rust-analyzer

;;; Code:

;; Base support
;; (use-package rust-mode)
(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(provide 'ys-rust-rustic)
;;; ys-rust-rustic.el ends here
