;;; ys-html.el --- HTML configuration (web mode)

;;; Commentary:

;;; Use web-mode for both html and javascript

;;; Code:

(require 'ys-editing)

;; Web mode is a special mode for HTML that handles embedded JS/CSS,
;; JSX, various templating systems, etc...
;; Ref: http://web-mode.org/
(use-package web-mode
  :mode ( ;; Use web-mode for HTML instead of the default html-mode.
         ("\\.html?\\'" . web-mode)
         ;; Further extensions detailed in web-mode docs
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  ;; Highlight the element under the cursor
  (setq-default web-mode-enable-current-element-highlight t)
  ;; Key for renaming tags
  (bind-keys :map web-mode-map
             ("C-c C-r" . 'mc/mark-sgml-tag-pair)))

;; Colourise coulour names in certain modes
(use-package rainbow-mode
  :config
  (dolist (mode '(css-mode less-css-mode html-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (rainbow-mode))))
  :diminish rainbow-mode)

(provide 'ys-html)
;;; ys-html.el ends here
