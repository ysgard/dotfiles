;;; ys-display.el --- Summary

;;; Commentary:

;;; Display-specific settings, such as fonts, screen size,
;;; toolbar/scrollbar settings, etc...

;; Set the initial frame size
;; We use the resolution threshold variable
;; to determine what size we should set
;; our editor to.

;;; Code:

;; Treat all themes as safe.  Don't really want to do this, but
;; second option to `load-theme` does bupkus.
(setq custom-safe-themes t)

;; Load theme.  Good options are monokai-theme, molokai-theme and
;; cyberpunk-theme
;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-one t))
;; (load-theme 'tronesque t)
;; (use-package cyberpunk-theme
;;   :config
;;   (load-theme 'cyberpunk t))

(use-package base16-theme
  :config
  (load-theme 'base16-default-dark t))

;; Set font according to the host we are on
(when (search "zen" (system-name))
  (defvar ysgard-font-face "Menlo")
  (defvar ysgard-font-size "12"))

(when (search "grove-f24" (system-name))
  (defvar ysgard-font-face "Consolas")
  (defvar ysgard-font-size "14"))

(when (search "GROVE" (system-name))
  (defvar ysgard-font-face "Consolas")
  (defvar ysgard-font-size "13"))

(when (search "pinkiepie" (system-name))
  (defvar ysgard-font-face "Hurmit Nerd Font Medium")
  (defvar ysgard-font-size "10"))

(when (search "shrine" (system-name))
  (defvar ysgard-font-face "Source Code Pro Medium")
  (defvar ysgard-font-size "10"))

(when (search "tombstone.local" (system-name))
  (defvar ysgard-font-face "Source Code Pro Light")
  (defvar ysgard-font-size "12"))

(when (search "jvanuytven-macbook-pro" (system-name))
  (defvar ysgard-font-face "Roboto Mono for Powerline")
  (defvar ysgard-font-size "14"))

;; Turn on line numbers, column numbers, and highlight current line
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-linum-mode t)
(setq column-number-mode 1)

;; Suppress the annoying startup message
;; Start in empty scratch buffer
(setq inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'text-mode)

;; Turn off toolbar and scrollbar, and disable menubar
;; when in terminal
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

;; Cosmetic enhancements
(setq x-underline-at-descent-lint t)
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil)

;; Always show filename in frame
(setq-default frame-title-format "%b (%f)")

;; Highlight matching parens
(show-paren-mode t)

;; Don't use standard audible and visible ring-bell function
;; the former is just annoying, the later does weird things
;; to the editor window
;; Instead, flash the mode-line
(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit
				      exit-minibuffer keyboard-quit))
	  (invert-face 'mode-line)
	  (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1))) ; five lines at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-step 5) ; keyboard scroll one line at a time

;; Set the font, if we've defined it
(if (boundp 'ysgard-font-face)
    (progn
      (defvar ysgard-font (concat ysgard-font-face " " ysgard-font-size))
      (add-to-list 'default-frame-alist (cons 'font ysgard-font))))

;; Set the initial frame size to something reasonable
(add-to-list 'initial-frame-alist '(width . 80))

(provide 'ys-display)
;;; ys-display.el ends here
