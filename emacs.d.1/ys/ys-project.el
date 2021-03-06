;;; ys-project.el --- Stuff for projects

;;; Commentary:

;; Mostly projectile.

;;; Code:

(use-package projectile
  :demand t
  :commands projectile-global-mode
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "s-j") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-l j") 'projectile-command-map)
  (define-key evil-motion-state-map (kbd "SPC j") 'projectile-command-map)
  :bind (("C-c C-f" . projectile-find-file)))

;; Use ibuffer instead of list-buffers and sort by project
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook (lambda ()
                            (ibuffer-projectile-set-filter-groups)
                            (unless (eq ibuffer-sorting-mode 'alphabetic)
                              (ibuffer-do-sort-by-alphabetic)))))

(provide 'ys-project)
;;; ys-project.el ends here
