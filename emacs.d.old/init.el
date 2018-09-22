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

(when (< emacs-major-version 24)
  (error "Need at least Emacs 24+ for these init scripts to work!"))

;; Base system initialization
(setq user-full-name "Jan Van Uytven")
(setq user-mail-address "ysgard@gmail.com")

;; Set the load path to include scripts in ~/.emacs.d
(add-to-list 'load-path "~/.emacs.d/ysgard")

;; Call the common lisp library (more code to play with, mmmm)
(require 'cl)

;; Call in some common utility functions - these should be function-agnostic, and
;; are used ONLY to augment common functionality, not control packages or emacs
;; behaviour.
(load "ysgard-defuns.el")

;; Load keybinds
(load "ysgard-keybinds.el")

;; Packages
(load "ysgard-packages.el")

;; Base settings (non-external)
(load "ysgard-base.el")

;; Display settings (eye candy!)
(load "ysgard-display.el")

;; Load trivial packages (must be loaded first)
(load "ysgard-misc.el")

;; Load settings for EVIL mode
(when (require 'evil nil 'noerror)
  (load "ysgard-evil.el"))

;; Load Emacs language server packages
;; (load "ysgard-lsp.el")

;; Load settings for Rust
(load "ysgard-rust.el")

;; Load settings for Haskell
(load "ysgard-haskell.el")

;; Load settings for Javascript
(load "ysgard-js.el")

;; Load setting for lisp/clojure
(load "ysgard-lisp.el")

;; Load settings for Clojure
(load "ysgard-clojure.el")

(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here
