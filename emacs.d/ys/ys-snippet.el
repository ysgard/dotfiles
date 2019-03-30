;;; ys-snippet.el --- Snippet and templates

;;; Commentary:

;;; Code:

(use-package yasnippet
  :config
  ;; personal snippets dir
  (setq yas-snippet-dirs (append yas-snippet-dirs "~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package yasnippet-snippets)



(provide 'ys-snippet)
;;; ys-snippet.el ends here
