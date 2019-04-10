;;;; ~/.emacs.d/init.el --- Summary

;;; All emacs initialization is controlled through here.
;;; Functionality is broken out into subfiles

;;; Commentary:

;;; Code:

;; Bail if we're not using a current version of emacs

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'cl-lib)

(when (or (< emacs-major-version 24)
          (and (= emacs-major-version 24) (< emacs-minor-version 4)))
  (x-popup-dialog
   t `(,(format "Sorry, you need GNU Emacs version 24.4 or higher 
to use these dotfiles.

You've got %s" (emacs-version))
       ("OK :(" . t)))
  (save-buffers-kill-emacs t))

;; Base system variables
(setq user-full-name "Jan Van Uytven")
(setq user-mail-address "ysgard@gmail.com")
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" ""
                                         (with-output-to-string
                                           (call-process "hostname" nil standard-output))))
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))

;; Call in the core - these files define the basic emacs experience
;; and sets up infrastructure for the misc modules to take advantage
;; of, in particular the package loading.
(add-to-list 'load-path "~/.emacs.d/thirdparty")
(add-to-list 'load-path "~/.emacs.d/ys")
(require 'ys-package)
(require 'ys-lib)
(require 'ys-base)
(require 'ys-display)
(require 'ys-evil)

;; Call in the misc modules - these are language or feature-specific
;; files that shouldn't depend on each other but might depend on
;; stuff in core.
(require 'ys-company)
(require 'ys-flycheck)
(require 'ys-lsp)
;; (require 'ys-rust-racer)
(require 'ys-rust-rls)
(require 'ys-lisp)
(require 'ys-magit)
(require 'ys-ido)
;; (require 'ys-narrows)
(require 'ys-treemacs)
(require 'ys-codestyle)
(require 'ys-javascript)
(require 'ys-markdown)
(require 'ys-smart-mode-line)
(require 'ys-emojify)
(require 'ys-yaml)
(require 'ys-c)
(require 'ys-ruby)
(require 'ys-project)
(require 'ys-dired)
(require 'ys-org)
(require 'ys-lua)
(require 'ys-terraform)
(require 'ys-zig)
(require 'ys-opengl)
(require 'ys-snippet)

(require 'ys-keybinds)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hcl-indent-level 4)
 '(package-selected-packages
   (quote
    (yasnippet-snippets yasnippet glsl-mode terraform-mode hcl-mode flymake-lua lua-mode ox-reveal org-cliplink org-bullets org-plus-contrib dired-icon dired-single ibuffer-projectile projectile inf-ruby rubocop flycheck-irony company-c-headers company-irony irony-eldoc yaml-mode emojify smart-mode-line rich-minority json-mode rainbow-mode web-mode tide ethan-wspace treemacs-evil treemacs flx-ido ido-vertical-mode smex ido-completing-read+ git-gutter-fringe gist magit racket-mode geiser slime-company use-package slime paredit paradox lsp-ui lsp-rust highlight-parentheses flycheck-rust flycheck-color-mode-line exec-path-from-shell evil-leader eros company-try-hard company-quickhelp company-lsp company-emoji cargo base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
