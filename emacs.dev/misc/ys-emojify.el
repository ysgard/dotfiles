;;; ys-emojify.el --- Support graphical emoji when font support is missing

;;; Commentary:

;;; Code:

(use-package emojify
  :config
  ;; Set emojify to only replace Unicode emoji, and do it everywhere.
  (setq emojify-emoji-styles '(unicode)
        emojify-inhibit-major-modes '())
  ;; Enable it globally
  (add-hook 'after-init-hook #'global-emojify-mode))

;; Patch emojify to replace emoji everywhere in programming modes.
(defun emojify-valid-prog-context-p (beg end) 't)

(provide 'ys-emojify)
;;; ys-emojify.el ends here
