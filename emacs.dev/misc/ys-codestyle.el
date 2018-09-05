;;; ys-codestyle.el --- Set code styling

;;; Commentary:

;;; Use spaces, not tabs, and 2-space indent should be the standard
;;; for everything not C, C++ or Rust.

;;; Code

;; No tabs.  Ever.
(setq-default indent-tabs-mode nil) ; Spaces instead of tabs

;; Sigh.  This is a difficult habit to break, but the unwashed masses have spoken.
;; Single spaces in sentences it is.
(setq sentence-end-double-space nil)

;; Indent after newlines
(define-key global-map (kbd "RET") 'newline-and-indent)

;; To help with the new 'one space after period' rule, and
;; general cleanliness With C-c c
(use-package ethan-wspace
  :demand t
  :commands global-ethan-wspace-mode
  :config
  (global-ethan-wspace-mode 1)
  :bind ("C-c c" . ethan-wspace-clean-all)
  :diminish ethan-wspace-mode)

(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; Defaults for various languages
(setq-default tab-width 2)

;; JS
(setq-default js2-basic-offset 2)

;; JSON
(setq-default js-indent-level 2)

;; Coffeescript
(setq coffee-tab-width 2)

;; Typescript
(setq typescript-indent-level 2
      typescript-expr-indent-offset 2)

;; Python
(setq-default py-indent-offset 2)

;; XML
(setq-default nxml-child-indent 2)

;; C
(setq-default c-basic-offset 4)

;; Text
(setq-default word-wrap t)
; (global-visual-line-mode t) ; Same, but breaks long lines into semantic units
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Ruby
(setq-default ruby-indent-level 2)

;; HTML with web-mode
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2)

;; Don't use GNU style (yuck), set C-style languages to use Java style
(setq c-default-style
      '((awk-mode ."awk")
        (other . "java")))


(provide 'ys-codestyle)
;;; ys-codestyle.el ends here
