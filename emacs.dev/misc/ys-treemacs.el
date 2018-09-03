;;; ys-treemacs.el --- Treemacs config

;;; Commentary:
;;;
;;; Treemacs is a nice file-browsing utility, similar
;;; to vim's NERDTree.

;;; Code:

(use-package treemacs
 :commands (treemacs)
 :bind ("C-l x" . treemacs)
 :hook (treemacs-mode . (lambda ()
                          (progn 
                            (local-set-key (kbd "k") #'treemacs-previous-line)
                            (local-set-key (kbd "j") #'treemacs-next-line)
                            (local-set-key (kbd "/") #'isearch-forward-regexp)))))

(use-package treemacs-evil
 :after (evil))

(provide 'ys-treemacs)
;;; ys-treemacs.el ends here
