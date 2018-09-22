;;; ys-org.el --- Org Mode configs

;;; Commentary:

;;; Code:
(use-package org
  :ensure org-plus-contrib
  :config
  ;; Stop org-mode from hijacking shift-cursor keys
  (setq org-replace-disputed-keys t)
  ;; Allow embedded graphviz code
  (org-babel-do-load-languages
   'org-babel-do-load-languages '((dot . t)))
  ;; Always use visual-line-mode in org-mode, and wrap at 80
  :hook (org-mode . (lambda ()
                      (visual-line-mode 1)
                      (set-visual-wrap-column 80))))

(use-package org-bullets
  :after (org)
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Insert links from clipboard
(use-package org-cliplink
  :after (org)
  :bind (:map org-mode-map ("C-c M-l" . org-cliplink)))








(provide 'ys-org)
;;; ys-org.el ends here