;;; ys-javascript.el --- The worse language ever, configuration

;;; Commentary:

;;; Uuuuuuuuugh

;;; Code:

;; If npm is installed, add its local prefix to the executable search path,
;; which will help find linters, etc...
(-when-let (npm-prefix (ys/exec-if-exec "npm" "config get prefix"))
  (setenv "PATH" (concat npm-prefix "/bin:" (getenv "PATH"))))

;; ;; Install js2-mode, a huge improvement over the default js mode
;; (use-package js2-mode
;;   :mode (("\\.js$" . js2-mode)
;;          ("\\.es6\\'" . js2-mode)
;;          ("\\.ejs\\'" . js2-mode))
;;   :interpreter "node"
;;   :commands js2-mode
;;   :config
;;   ;; Leverage js2-mode to get refactoring support through js2-refactor
;;   (use-package js2-refactor
;;     :commands (js2r-add-keybindings-with-prefix)
;;     :init
;;     (add-hook 'js2-mode-hook #'js2-refactor-mode)
;;     (js2r-add-keybindings-with-prefix "C-c C-m"))
;;   (setq-default
;;    js2-mode-indent-ignore-first-tab t
;;    js2-strict-inconsistent-return-warning nil
;;    js2-global-externs
;;    '("module" "require" "__dirname" "process" "console" "JSON" "$" "_"))
;;    ;; js2-show-parse-errors nil
;;    ;; js2-strict-var-hides-function-arg-warning nil
;;    ;; js2-strict-missing-semi-warning nil
;;    ;; js2-strict-trailing-comma-warning nil
;;    ;; js2-strict-cond-assign-warning nil
;;    ;; js2-strict-var-redeclaration-warning nil
;;   )

;; Use Tern for smarter JS
;; (use-package tern
;;   :commands tern-mode
;;   :config
;;   (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;;   ;; Locate the tern binary
;;   (setq tern-command (list (or (ys/resolve-exec "tern") "tern")))
;;   ;; Setup tern as an autocomplete source
;;   (with-eval-after-load "company"
;;     (use-package company-tern
;;       :config
;;       (add-to-list 'company-backends 'company-tern))))

;; Typescript-mode
(defun setup-tide-mode ()
  (interactive)
  ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbvose -file /tmp/tss.log"))
  (tide-setup)
  (if (file-exists-p (concat tide-project-root "node_modules/typescript/bin/tsserver"))
      (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
    (setq tide-tsserver-executable "~/.config/yarn/global/node_modules/.bin/tsserver"))
  (flycheck-mode +1)
  (tide-hl-identifier-mode +1)
  (setq tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  (local-set-key (kbd "C-c d") 'tide-documentation-at-point))

(use-package tide
  :config
  (setq company-tooltip-align-annotations t)
  :hook (typescript-mode . setup-tide-mode)
  :mode ("\\.ts\\'" . typescript-mode))

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
         ("\\.djhtml\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.js" . web-mode))
  :config
  ;; Highlight the element under the cursor
  (setq-default web-mode-enable-current-element-highlight t)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  ;; Key for renaming tags
  (bind-keys :map web-mode-map
             ("C-c C-r" . 'mc/mark-sgml-tag-pair))
  :hook (web-mode-hook . (lambda ()
                           (setq web-mode-code-indent-offset 2)
                           (let ((extension (file-name-extension buffer-file-name)))
                             (when (or (string-equal "tsx" extension)
                                       (string-equal "jsx" extension))
                               (setup-tide-mode))
                             (when (string-equal "js" extension)
                               (progn
                                 (setup-tide-mode)
                                 (with-eval-after-load 'flycheck
                                   (flycheck-add-mode 'typescript-tsline 'web-mode)
                                   (flycheck-add-mode 'javascript-tide 'web-mode))))))))

(add-to-list 'exec-path "~/.config/yarn/global/node_modules/.bin")

;; Colourise coulour names in certain modes
(use-package rainbow-mode
  :config
  (dolist (mode '(css-mode less-css-mode html-mode web-mode))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (rainbow-mode))))
  :diminish rainbow-mode)

(use-package json-mode
  :commands json-mode
  :config
  (bind-keys :map json-mode-map
             ("C-c <tab>" . json-mode-beautify))
  (flycheck-add-mode 'json-jsonlint 'json-mode)
  (setq js-indent-level 2)
  :mode "\\.json"
  :hook (json-mode . flycheck-mode))

(provide 'ys-javascript)
;;; ys-javascript.el ends here
