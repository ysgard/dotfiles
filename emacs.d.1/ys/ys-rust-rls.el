;;; ys-rust-rls.el --- Summary

;;; Rust support for Emacs

;;; Commentary:

;;; Uses the rls, and works, but I find the rls too 'noisy' to use, and prefer
;;; the more unobtrusive racer.

;;; WARNING: Use this module, or ys-rust-racer.el, but not both.

;;; Code:

;; rust-mode
(use-package rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-ask-about-save nil)
  (add-hook 'cargo-minor-mode 'ansi-color-for-comint-mode-on)
  :bind (("C-c C-b" . cargo-process-build)
         ("C-c C-r" . cargo-process-run)
         ("C-c C-t" . cargo-process-test)
         ("C-c C-m" . cargo-process-clean))
  :diminish cargo-minor-mode)

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; If the LSP module is enabled, set up RLS support
(with-eval-after-load 'lsp-mode
  (require 'ys-flycheck)
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (use-package lsp-rust
    :hook ((rust-mode . lsp-rust-enable)
           (rust-mode . flycheck-mode))))

(provide 'ys-rust-rls)
;;; ys-rust-rls.el ends here
