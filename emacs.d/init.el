;;;; ~/.emacs.d/init.el --- Summary

;;; Ysgard's Emacs.d init.el

;;; Commentary:

;;; This one is mine.

;;; Sections:
;;; PRELUDE
;;; PACKAGE
;;; DISPLAY
;;; EVIL
;;; IDO
;;; COMPANY
;;; FLYCHECK
;;; DIRED
;;; MAGIT
;;; ORG
;;; LSP
;;; LANGUAGES
;;; MISC
;;; KEYBINDS


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

;; make sure buffers match file contents file changes
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq-default tab-width 4
	          indent-tabs-mode nil) ; spaces instead of tabs

;; Indent after newlines
(define-key global-map (kbd "RET") 'newline-and-indent)

(setq inhibit-startup-message t) ; suppress startup message
(setq-default frame-title-format "%b (%f)") ; show filename in frame

(setq sentence-end-double-space nil) ; The unwashed masses have spoken, no double spaces

(setq custom-safe-themes t) ; All themes are safe

;; Make sure our $PATH matches the system's
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Trim whitespace on save, always new line at EOF
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)


;;; DISPLAY
;;;
;;; Make Emacs look purdy

;; (use-package base16-theme
;;   :config
;;   (load-theme 'base16-default-dark t))
;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-vibrant))
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium))


(when (cl-search "pinkiepie" (system-name))
  (defvar ysgard-font-face "Fira Code Medium")
  (defvar ysgard-font-size "10"))

(when (cl-search "jvanuytven-macbook-pro" (system-name))
  (defvar ysgard-font-face "Cascadia Mono PL")
  (defvar ysgard-font-size "12"))

;; Turn on line numbers, column numbers, and highlight current line
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-linum-mode t)
(setq column-number-mode t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; disables tool bar
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; disables scroll bar
(unless (display-graphic-p) (menu-bar-mode -1)) ; turn off menu in terminal

(setq x-underline-at-descent-line t)
(setq use-dialog-box nil) ; We use keys for everything, no mouse if possible

(show-paren-mode t) ; Highlight matching parens

;; Load Fira Code
(ys/with-system gnu/linux
  (require 'ys-fira-code))
;;(ys/with-system darwin
;;  (max-auto-operator-composition-mode))

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

;; Interpret color codes in shells
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


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
(cl-loop for (mode . state) in
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
(ido-mode t)
(setq ido-enable-prefix nil ; finds matches that aren't prefixes
      ido-enable-flex-matching t ; match characters if can't match substring
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-max-prospects 10
      ido-use-virtual-buffers t)

;; Ido everywhere!
(use-package ido-completing-read+
  :config (ido-ubiquitous-mode t))

;; Smex provides an ido-like interface to M-x
(use-package smex
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; Old M-x
         ("C-c C-c M-x" . execute-extended-command)))

(use-package ido-vertical-mode
  :config (ido-vertical-mode))

;; Improved fuzzy-finding with flx
(use-package flx-ido
  :config
  (flx-ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-faces nil
        gc-cons-threshold 20000000))

;; Bind '~' to go to homedir when in ido-find-file
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

;;; COMPANY
;;;
;;; Try to use Company for all autocomplete

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-global-modes '(not term-mode)) ; no company in terminals
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  ;; Company default colors look pretty bad in dark mode, use something nicer
  (set-face-foreground 'company-tooltip "#000")
  (set-face-background 'company-tooltip "#ddd")
  (set-face-background 'company-scrollbar-bg "#fff")
  (set-face-background 'company-scrollbar-fg "#999")
  (set-face-background 'company-tooltip-selection "#aaa")
  (set-face-foreground 'company-tooltip-common "#9a0000")
  (set-face-foreground 'company-tooltip-common-selection "#9a0000")
  (set-face-foreground 'company-tooltip-annotation "#00008e")
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


;;; FLYCHECK
;;;
;;; Spot errors on the fly

(use-package flycheck
  :config
  ;; Don't enable flycheck for elisp, it's dumb
  :hook (find-file-hook . (lambda () (when (not (equal 'emacs-lisp-mode major-mode))
                                         (flycheck-mode)))))

;; Turn the modeline red when flycheck spots errors
(use-package flycheck-color-mode-line
  :after (flycheck)
  :config (setq flycheck-highlighting-mode 'symbols)
  :hook (flycheck-mode-hook . flycheck-color-mode-line))

(with-eval-after-load "flycheck"
  (set-face-background 'flycheck-error "#660000")
  (set-face-foreground 'flycheck-error nil)
  (set-face-background 'flycheck-warning "#331800")
  (set-face-foreground 'flycheck-warning nil)
  (require 'flycheck-color-mode-line)
  (set-face-background 'flycheck-color-mode-line-error-face "#440000")
  (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
  (set-face-background 'flycheck-color-mode-line-info-face nil)
  (set-face-foreground 'flycheck-color-mode-line-error-face "#ffffff")
  (set-face-foreground 'flycheck-color-mode-line-warning-face "#ffffff")
  (set-face-foreground 'flycheck-color-mode-line-info-face nil))


;;; DIRED
;;;
;;; Built-in File Manager options

(require 'dired)
;; Reuse dired buffers and avoid unnecessary proliferation
(use-package dired-single
  :config
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer))

(use-package dired-icon
  :hook ((dired-mode . dired-icon-mode)))



;;; MAGIT
;;;
;;; Superior git-mode
(use-package magit
  :commands magit-status)

(use-package gist)

;; Mark uncommitted changes in the fringe
(use-package git-gutter-fringe
  :config (global-git-gutter-mode t)
  :diminish git-gutter-mode)


;;; ORG
;;;
;;; Ultimate note-taking

(use-package org
  :ensure org-plus-contrib
  :config
  ;; Stop org-mode from hijacking shift-cursor key
  (setq org-replace-disputed-keys t)
  (setq org-hide-emphasis-markers t) ; hide emphasis markers
  (org-babel-do-load-languages
   'org-babel-load-languages '((dot . t))) ; allow embedded graphviz code
  :hook (org-mode . (lambda () (org-indent-mode t))))

(use-package org-bullets
  :after (org)
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Write reveal presentations using org-mode
(use-package ox-reveal)

;; Org-brain (https://github.com/Kungsgeten/org-brain)
(use-package org-brain :ensure t
  :after (org)
  :init
  (setq org-brain-path "~/Notes")
  ;; Evil
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  ; (push '("b" "Brain" plain (function org-brain-goto-end)
  ;        "* %i%?" :empty-lines 1)
  ;      org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;; Deft (https://jblevins.org/projects/deft/)
(use-package deft
  :config
  (setq deft-extensions '("txt" "tex" "org" "md" "markdown" "text")
        deft-directory "~/Notes"
        deft-recursive t)
  :commands (deft)
  :bind ("<f8>" . deft))


;;; LSP
;;;
;;; Language server protocol, general options

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;;; LANGUAGES
;;;
;;; Options for specific languages

;; C, C++, Objective-C
;;

(require 'lsp-mode)
(require 'lsp-ui)

;; Python

;; Ruby
(use-package enh-ruby-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package rspec-mode)

(use-package rubocop
  :hook (ruby-mode . rubocop-mode)
  :diminish rubocop-mode)

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

;; Rust
(add-hook 'rust-mode-hook #'lsp)
(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer))

;; Terraform
(use-package hcl-mode
  :config (custom-set-variables '(hcl-indent-level 2))
  :mode "\\.hcl\\'")

(use-package terraform-mode
  :config (custom-set-variables '(terraform-indent-level 2))
  :mode "\\.tf\\'")

;; YAML
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

;; Zig
(use-package zig-mode
  :mode "\\.zig\\'")

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  (add-hook 'go-mode-hook (lambda () (display-line-numbers-mode 1)))
  :bind ("C-c b" . compile)
  :hook ((go-mode . lsp-deferred)
         (go-mode . yas-minor-mode)))


(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;; MISC
;;;
;;; Miscellaneous tweaks and extensions go here

;; Treemacs
(use-package treemacs
  :commands (treemacs)
  ;; Rebind k/j and / to match vim, because the buffer reacts poorly with evil in general
  :hook (treemacs-mode . (lambda ()
                           (progn
                             (local-set-key (kbd "k") #'treemacs-previous-line)
                             (local-set-key (kbd "j") #'treemacs-next-line)
                             (local-set-key (kbd "/") #'isearch-forward-regexp)))))

;; Snippets
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :config
  (setq yas-snippet-dirs
        (append yas-snippet-dirs (concat init-dir "snippets"))) ; personal snippet dir
  (yas-global-mode t))
(use-package yasnippet-snippets)

;; Cleaner mode line
(use-package rich-minority
  :commands rich-minority-mode
  :demand t
  :init
  (setq rm-blacklist '(" Helm" " Guide" " $" " ," " Tern" " Ind" " alchemist" " Monroe" " clrj" " Wrap" " Doc"))
  :config
  (rich-minority-mode t))

(use-package smart-mode-line
  :commands sml/setup
  :demand t
  :init
  (setq sml/theme 'respectful
        sml/shorten-directory t
        sml/name-width 40
        sml/mode-width 'full)
  :config
  (sml/setup))


;; Crux - very useful commands
;; See https://github.com/bbatsov/crux
(use-package crux)


;; Avy - jumpt to visible text with chars
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)))

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
(define-key evil-motion-state-map (kbd "SPC f") 'ido-find-file)
(define-key evil-motion-state-map (kbd "SPC `") 'ys/eshell-here)
(define-key evil-motion-state-map (kbd "SPC m") 'magit-status)
(define-key evil-motion-state-map (kbd "SPC d") 'dired-jump)
(define-key evil-motion-state-map (kbd "SPC x") 'treemacs)
(define-key evil-motion-state-map (kbd "SPC c r") 'rustic-popup)


(global-set-key (kbd "C-x C-r") 'ys/rename-current-buffer-file)




;;; init.el ends here



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))
 '(hcl-indent-level 2)
 '(package-selected-packages '(flycheck-nim nim-mode base16-theme use-package))
 '(tramp-syntax 'default nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
