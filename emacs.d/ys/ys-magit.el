;;; ys-magit.el --- Install and configure magit

;;; Commentary:

;;; Code:

(use-package magit
  :commands magit-status)

;; Create gitst easily with M-x gist-buffer or M-x gist-region
(use-package gist)

;; Mark uncommited changes in the fringe
(use-package git-gutter-fringe
  :config (global-git-gutter-mode t)
  :diminish git-gutter-mode)

(provide 'ys-magit)
;;; ys-magit.el ends here
