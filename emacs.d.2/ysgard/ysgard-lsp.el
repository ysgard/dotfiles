;;; ysgard-lsp.el --- Summary

;;; Language Server Support

;;; Commentary:

;; See: https://github.com/emacs-lsp/lsp-mode
;;      https://github.com/emacs-lsp/lsp-ui

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lsp-mode")
(require 'lsp-mode)
(add-to-list 'load-path "~/.emacs.d/lsp-ui")
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(require 'company-lsp)
(push 'company-lsp company-backends)

;; Rust
(add-to-list 'load-path "~/.emacs.d/lsp-rust")
(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (require 'lsp-rust))

(add-hook 'rust-mode-hook #'lsp-rust-enable)
(add-hook 'rust-mode-hook #'flycheck-mode)



;; (lsp-define-stdio-client
 ;; This can be a symbol of your choosing. It will be used as a the
 ;; prefix for a dynamically generated function "-enable"; in this
 ;; case: lsp-prog-major-mode-enable
;; lsp-prog-major-mode
;; "language-id"
 ;; This will be used to report a project's root directory to the LSP
 ;; server.
;; (lambda () default-directory)
 ;; This is the command to start the LSP server. It may either be a
 ;; string containing the path of the command, or a list wherein the
 ;; car is a string containing the path of the command, and the cdr
 ;; are arguments to that command.
;; '("/my/lsp/server" "and" "args"))

;; Here we'll add the function that was dynamically generated by the
;; call to lsp-define-stdio-client to the major-mode hook of the
;; language we want to run it under.
;;
;; This function will turn lsp-mode on and call the command given to
;; start the LSP server.
;;(add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

(provide 'ysgard-lsp)
;;; ysgard-lsp.el ends here
