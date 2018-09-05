;;; ys-javascript.el --- The worse language ever, configuration

;;; Commentary:

;;; Uuuuuuuuugh

;;; Code:

(require 'ys-json)
(require 'ys-lib)

;; If npm is installed, add its local prefix to the executable search path,
;; which will help find linters, etc...
(-when-let (npm-prefix (ys/exec-if-exec "npm" "config get prefix"))
  (setenv "PATH" (concat npm-prefix "/bin:" (getenv "PATH"))))

;; Install js2-mode, a huge improvement over the default js mode
(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("\\.es6\\'" . js2-mode)
         ("\\.ejs\\'" . js2-mode))
  :interpreter "node"
  :commands js2-mode
  :config
  ;; Leverage js2-mode to get refactoring support through js2-refactor
  (use-package js2-refactor
    :commands (js2r-add-keybindings-with-prefix)
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybinding-with-prefix "C-c C-m"))
  (setq-default
   js2-mode-indent-ignore-first-tab t
   js2-strict-inconsistent-return-warning nil
   js2-global-externs
   '("module" "require" "__dirname" "process" "console" "JSON" "$" "_"))
   ;; js2-show-parse-errors nil
   ;; js2-strict-var-hides-function-arg-warning nil
   ;; js2-strict-missing-semi-warning nil
   ;; js2-strict-trailing-comma-warning nil
   ;; js2-strict-cond-assign-warning nil
   ;; js2-strict-var-redeclaration-warning nil
  )

;; Use Tern for smarter JS
(use-package tern
  :commands tern-mode
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  ;; Locate the tern binary
  (setq tern-command (list (or (ys/resolve-exec "tern") "tern")))
  ;; Setup tern as an autocomplete source
  (with-eval-after-load "company"
    (use-package company-tern
      :config
      (add-to-list 'company-backends 'company-tern))))

(provide 'ys-javascript)
;;; ys-javascript.el ends here
