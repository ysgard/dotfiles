;;;; ~/.emacs.d/init.el --- Summary

;;; Ysgard's Emacs.d init.el

;;; Commentary:

;;; This one is mine.

;;; Code:

;;; PRELUDE

(require 'cl-lib)

;; Bail if we're not running a modern version of Emacs

(when (< emacs-major-version 26)
  (x-popup-dialog
   t `(,(format "Version 26 of Emacs required! You have version %s" (emacs-version))
       ("OK" . t)))
  (save-buffers-kill-emacs t))

;; Set some base information

(setq user-full-name "Jan Van Uytven")
(setq user-mail-address "ysgard@gmail.com")
(setq init-dir (file-name-directory
		(or (buffer-file-name) (file-chase-links load-file-name))))

(add-to-list 'load-path (concat init-dir "/ys"))
(require 'ys-lib)

;;; PACKAGE
;;;
;;; Set up the package manager (we use use-package)

(setq package-user-dir (concat init-dir "elpa"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;; If we're online, fetch package directories and refresh them
(when (ys-online?) (package-refresh-contents))

;; Install use-package, we use it to install everything else
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)
;; Auto-install declared packages
(setq use-package-always-ensure t)


;;; BASE CONFIGURATION
;;;
;;; Configure base Emacs

(defalias 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t) ;typed text replaces selection
(transient-mark-mode t)   ;highlight marked regions

;; UTF-8 for everything
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")
(require 'iso-transl)

;; Turn off backups and deal with temp files
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq apropos-do-all t) ; apropos searches all
(global-auto-revert-mode t) ; make sure buffers match file contents file changes

(setq-default tab-width 4
	      indent-tabs-mode nil) ; spaces instead of tabs

(setq inhibit-startup-message t) ; suppress startup message
(setq-default frame-title-format "%b (%f)") ; show filename in frame

;;; DISPLAY
;;;
;;; Make Emacs look purdy

(use-package base16-theme
  :config
  (load-theme 'base16-default-dark t))

(when (search "pinkiepie" (system-name))
  (defvar ysgard-font-face "Hurmit Nerd Font Medium")
  (defvar ysgard-font-size "10"))

;; Turn on line numbers, column numbers, and highlight current line
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-linum-mode t)
(setq column-number-mode t)

(tool-bar-mode t) ; disables tool bar
(scroll-bar-mode t) ; disables scroll bar
(unless (display-graphic-p) (menu-bar-mode t)) ; turn off menu in terminal

(setq x-underline-at-descent-line t)
(setq use-dialog-box nil) ; We use keys for everything, no mouse if possible

(show-paren-mode t) ; Highlight matching parens

;; Flash mode line when we 'ring the bell'
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit
                                      exit-minibuffer keyboard-quit))
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; We want smooth scrolling
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1))) ; 5 lines at a time
(setq mouse-wheel-progressive-speed nil) ; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; Scroll window under mouse
(setq scroll-step 5) ; keyboard scroll one line at a time

;; Set the font, if we've defined it
(if (boundp 'ysgard-font-face)
    (progn
      (defvar ysgard-font (concat ysgard-font-face " " ysgard-font-size))
      (add-to-list 'default-frame-alist (cons 'font ysgard-font))))

(add-to-list 'initial-frame-alist '(width . 100)) ; Set default frame width


;;; EVIL
;;;
;;; Configure Evil-mode
(use-package evil)
(use-package evil-leader)
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)

;; Set comma to be leader
(evil-leader/set-leader ",")
(setq evil-leader/in-all-states t)
(evil-mode t)

;; Set cursor colors so we know what mode we're in
(setq evil-emacs-state-cursor '("red" box)
      evil-normal-state-cursor '("green" box)
      evil-visual-state-cursor '("orange" box)
      evil-insert-state-cursor '("red" bar)
      evil-replace-state-cursor '("red" bar)
      evil-operator-state-cursor '("red" hollow))

;; Define initial states for certain modes
;; Some modes, like Dired and the shell, deal poorly with evil
(loop for (mode . state) in
      '((inferior-emacs-lisp-mode . emacs)
        (nrepl-mode . insert)
        (comint-mode . normal)
        (shell-mode . emacs)
        (git-commit-mode . insert)
        (term-mode . emacs)
        (help-mode . emacs)
        (grep-mode . emacs)
        (bc-menu-mode . emacs)
        (magit-branch-manager-mode . emacs)
        (dired-mode . normal)
        (ibuffer-mode . normal)
        (wdired-mode . normal)
        (treemacs-mode . emacs)
        (rustic-popup-mode . emacs))
      do (evil-set-initial-state mode state))

;;; IDO
;;;
;;; Ido forever!



;;; COMPANY
;;;
;;; Try to use Company for all autocomplete

(use-package company
  :demand t
  :commands company-mode
  :config
  (global-company-mode)
  (setq company-global-modes '(not term-mode)) ; no company in terminals
  (setq company-transformers '(company-sort-by-occurence)) ; in-buffer symbols appear first
  ;; Company default colors look pretty bad in dark mode, use something nicer
  (set-face-foreground 'company-tooltip "#000")
  (set-face-background 'company-tooltip "#ddd")
  (set-face-background 'company-scrollbar-bg "#fff")
  (set-face-background 'company-scrollbar-fg "#999")
  (set-face-background 'company-tooltip-selection "#aaa")
  (set-face-foreground 'company-tooltip-common "#9a0000")
  (set-face-foreground 'company-tooltop-common-selection "#9a0000")
  (set-face-foreground 'company-tooltop-annotation "#00008e")
  :diminish company-mode)

(use-package company-quickhelp
  :after (company)
  :config
  (setq company-quickhelp-delay 1)
  (company-quickhelp-mode t))

(use-package company-emoji
  :after (company)
  :config
  (company-emoji-init))



;;; KEYBINDS
;;;
;;; Custom keybinds

;; Unset space in evil normal mode so we can use it as a prefix
(define-key evil-motion-state-map " " nil)

(define-key evil-motion-state-map (kbd "SPC b") 'ibuffer)
(define-key evil-motion-state-map (kbd "SPC o") 'other-window)
(define-key evil-motion-state-map (kbd "SPC k") 'kill-this-buffer)
(define-key evil-motion-state-map (kbd "SPC n") 'ys/next-user-buffer)
(define-key evil-motion-state-map (kbd "SPC p") 'ys/previous-user-buffer)
(define-key evil-motion-state-map (kbd "SPC s") 'save-buffer)
(define-key evil-motion-state-map (kbd "SPC f") 'find-file)
(define-key evil-motion-state-map (kbd "SPC `") 'ys/eshell-here)
(define-key evil-motion-state-map (kbd "SPC x x") 'ys/exec)
;;; init.el ends here
  
		      

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (base16-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
