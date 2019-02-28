;;; ys-lisp.el --- Summary

;;; Lisp hacks 

;;; Commentary:

;; Also includes stuff for Scheme, namely Racket.

;;; Code:

(setq lisp-indent-function 'common-lisp-indent-function)

;; Learn Paredit: http://pub.gajendra.net/src/paredit-refcard.pdf
(use-package paredit
  :commands paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  ;; Setup C-c v to eval whole buffer in all lisps
  (define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
  :diminish paredit-mode)

;; Highlight sexp under the cursor
(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :config
  (add-hook 'emacs-lispmode-hook 'highlight-parentheses-mode)
  (add-hook 'lisp-mode-hook 'highlight-parentheses-mode)
  (add-hook 'scheme-mode-hook 'highlight-parentheses-mode)
  :diminish highlight-parentheses-mode)

;; When saving an elisp file, remove its compiled version
(defun ys/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'ys/remove-elc-on-save)

;; 


;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Shows you the result of evaluating an elisp command as an overlay in your elisp buffer.
;; Try it out with C-x C-e
(use-package eros
  :commands eros-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eros-mode))

;; Use M-. to jump to the definition of a symbol under the cursor
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; Use SLIME to interact with SBCL/Quicklisp
;; Make sure you install the quicklisp package :quicklisp-slime-helper
(use-package slime
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))
  (setq slime-contribs '(slime-fancy))
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-repl
                 slime-fuzzy
                 slime-fancy-inspector
                 slime-indentation))
  (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol))




  
(use-package slime-company
  :after (slime company)
  :config
  (slime-setup '(slime-company)))

;; Scheme stuff

(use-package geiser)
(use-package racket-mode)

(provide 'ys-lisp)
;;; ys-lisp.el ends here
