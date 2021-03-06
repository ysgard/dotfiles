;;; ys-org.el --- Org Mode configs

;;; Commentary:

;;; Code:
(use-package org
  :ensure org-plus-contrib
  :config
  ;; Stop org-mode from hijacking shift-cursor keys
  (setq org-replace-disputed-keys t)
  ;; Hide emphasis markers
  (setq org-hide-emphasis-markers t)
  ;; Allow embedded graphviz code
  (org-babel-do-load-languages
   'org-babel-load-languages '((dot . t)))
  ;; Always use visual-line-mode in org-mode, and wrap at 80
  :hook (org-mode . (lambda () (org-indent-mode t))))

(use-package org-bullets
  :after (org)
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Insert links from clipboard
(use-package org-cliplink
  :after (org)
  :bind (:map org-mode-map ("C-c M-l" . org-cliplink)))

;; Write reveal presentations using org-mode
(use-package ox-reveal)

;; Unbind alt-<arrow> keys to next-buffer and prev-buffer,
;; as they have special meaning in org and it interferes with 
;; the general navigation of buffers
(define-key org-mode-map (kbd "M-<right>") nil)
(define-key org-mode-map (kbd "M-<left>") nil)

(provide 'ys-org)
;;; ys-org.el ends here
