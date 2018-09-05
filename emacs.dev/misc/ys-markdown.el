;;; ys-markdown.el --- Markdown packages and config

;;; Commentary:

;;; As Bodil notes - "That text format that's everywhere org-mode should be"


;;; Code:

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(provide 'ys-markdown)
;;; ys-markdown.el ends here
