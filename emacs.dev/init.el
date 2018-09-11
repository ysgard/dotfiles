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
(require 'cl)

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
(add-to-list 'load-path "~/.emacs.d/ys")
(require 'ys-package)
(require 'ys-lib)
(require 'ys-base)
(require 'ys-display)

;; Call in the misc modules - these are language or feature-specific
;; files that shouldn't depend on each other but might depend on
;; stuff in core.
(add-to-list 'load-path "~/.emacs.d/misc")
(require 'ys-evil)
(require 'ys-company)
(require 'ys-flycheck)
(require 'ys-lsp)
(require 'ys-rust)
(require 'ys-lisp)
(require 'ys-magit)
(require 'ys-ido)
;; (require 'ys-narrows)
(require 'ys-treemacs)
(require 'ys-codestyle)
(require 'ys-html)
(require 'ys-javascript)
(require 'ys-markdown)
(require 'ys-smart-mode-line)
(require 'ys-emojify)
(require 'ys-yaml)
(require 'ys-c)

(provide 'init)
;;; init.el ends here
