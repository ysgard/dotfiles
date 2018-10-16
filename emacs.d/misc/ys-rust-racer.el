;;; ys-rust.el --- Summary

;;; Rust support for Emacs

;;; Commentary:

;;; Rust support, using racer.  I prefer this to rls, as I find rls both too
;;; noisy and too inaccurate.  If my autocomplete is not going to be complete,
;;; I'd prefer racer's more unobtrusive approach.

;;; WARNING: Use this module, or ys-rust-rls.el, but not both.

;;; Code:

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

(use-package racer
  :after (company)
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)
         (racer-mode . company-mode))
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package flycheck-rust
  :after (rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'ys-rust-racer)
;;; ys-rust.el ends here
