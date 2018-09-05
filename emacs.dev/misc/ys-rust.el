;;; ys-rust.el --- Summary

;;; Rust support for Emacs

;;; Commentary:

;;; All the goodness needed for Rust
;;; rust-mode - highlighting and cargo support
;;; 

;;; Code:

;; rust-mode
(use-package rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-ask-about-save nil)
  :bind (("C-c C-b" . cargo-process-build)
         ("C-c C-r" . cargo-process-run)
         ("C-c C-t" . cargo-process-test)
         ("C-c C-m" . cargo-process-clean))
  :diminish cargo-minor-mode)

;; If the LSP module is enabled, set up RLS support
(with-eval-after-load "ys-lsp"
  (require 'ys-flycheck)
  (use-package lsp-rust
    :hook ((rust-mode . lsp-rust-enable)
           (rust-mode . flycheck-mode))))

(provide 'ys-rust)
;;; ys-rust.el ends here
