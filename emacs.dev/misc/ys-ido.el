;;; ys-ido.el --- Fuzzy-finding

;;; Commentary:

;; Is it possible to use both helm and ido?  Don't know.

;;; Code:

(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-max-prospects 10
      ido-use-virtual-buffers t)

;; Ido everywhere!
(use-package ido-completing-read+
  :config (ido-ubiquitous-mode 1))

;; smex provides an ido-like interface to M-x

(use-package smex
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; Old M-x
         ("C-c C-c M-x" . execute-extended-command)))


;; Vertical ido
(use-package ido-vertical-mode
  :config (ido-vertical-mode))

;; Improved fuzzy matching with flx
(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-faces nil
        ;; Not sure why we should set a threshold here, following
        ;; ohai's example
        gc-cons-threshold 20000000))

;; Bind C-t to use ido to jump to a symbol in the current buffer.
(require 'imenu)
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to. Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu-make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(set-default 'imenu-auto-rescan t)
(global-set-key (kbd "C-t") 'ido-menu)

;; Bind '~' to go to homedir when in ido-find-file.
;; From http://whattheemacsd.com/setup-ido.el-02.html
(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

(provide 'ys-ido)
;;; ys-ido.el ends here
