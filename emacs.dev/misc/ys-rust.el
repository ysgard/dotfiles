;;; ys-rust.el --- Summary

;;; Rust support for Emacs

;;; Commentary:

;;; All the goodness needed for Rust
;;; rust-mode - highlighting and cargo support
;;; 

;;; Code:

;; rust-mode
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; paths
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/src/rust/src")

;; Racer support
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;; cargo
(when (require 'cargo nil 'noerror)
  (progn
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    (add-hook 'cargo-minor-mode-hook
              (lambda ()
                (progn
                  (local-set-key (kbd "C-c C-b") 'cargo-process-build)
                  (local-set-key (kbd "C-c C-r") 'cargo-process-run)
                  (local-set-key (kbd "C-c C-t") 'cargo-process-test)
                  (local-set-key (kbd "C-c C-m") 'cargo-process-clean))))))


;; flycheck-rust
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Company config
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(provide 'ys-rust)
;;; ys-rust.el ends here
