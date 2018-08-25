;;; ~/.emacs.d/package.el --- Summary: Packages to install

;;; Commentary:

;;; This file defines the packages that we want to load
;;; into Emacs, as well as the functions needed to
;;; get and load them automatically on init

;;; Code:

;; Current list of packages
(defvar ysgard/packages '(
                          abyss-theme                    ; Dark theme with strong colors, based on lush
                          alect-themes
                          auto-complete
                          blackboard-theme
                          cargo                          ; Support for Rust's package manager
                          cider                          ; REPL for Clojure
                          clojure-mode                   ; Clojure lisp syntax highlighting
                          clojure-mode-extra-font-locking
                          color-theme-modern
                          color-theme-sanityinc-tomorrow ; Tomorrow themes
                          company                        ; text completion framework
                          company-ghci
                          cql-mode                       ; Cassandra
                          cyberpunk-theme
                          dash                           ; modern list library
                          d-mode                         ; Dlang integration
                          dtrt-indent                    ; automatically adjust indent per file
                          docker                         ; docker goodness in Emacs
                          dockerfile-mode                ; edit dockerfiles in style
                          evil                           ; vim support!
                          evil-leader                    ; Allow us to set vim leader
                          exec-path-from-shell           ; Pull shell vars in OS X
                          fireplace                      ; cozy little fireplace
                          flx-ido                        ; Fuzzy-finding for ido
                          flycheck                       ; syntax checking and highlightin
                          flycheck-rust
                          ghc                            ; Sub-mod for haskell
                          github-theme
                          google-maps                    ; Load a map of a given location
                          google-this                    ; Google lookup under pointq
                          groovy-mode                    ; Groovy mode
                          hydra                          ; Tie related commands into a family of short bindings with a common prefix - a hydra
                          ido-completing-read+           ; Replaced ido-ubiquitous
                          ido-vertical-mode              ; Make ido display vertically
                          intero                         ; Haskell
                          js2-mode                       ; Improved JavaScript major mode
                          json-mode                      ; JSON
                          key-chord                      ; Allows multi-key chording
                          kibit-helper                   ; kibit is a static code analyzer for Clojure
                          leuven-theme
                          linum-relative                 ; relative line numbers, a la vim
                          magit                          ; git integration
                          markdown-mode                  ; Markdown mode & preview
                          material-theme
                          mode-icons                     ; icons for major modes
                          moe-theme
                          monokai-theme                  ; best theme
                          molokai-theme
                          multi-term                     ; terminal in separate buffer
                          nord-theme
                          occidental-theme
                          oldlace-theme
                          org                            ; Latest version
                          org-bullets                    ; Nice bullets for org-mode
                          ;; paredit                     ; Pretty much needed for Lisp
                          perspective                    ; Compartemalize projects per window?
                          professional-theme
                          racer                          ; Racer support for rust
                          rainbow-delimiters             ; All the colors of parentheses!
                          rust-mode                      ; Rust support
                          silkworm-theme
                          soft-stone-theme
                          smart-mode-line                ; Nice status line
                          smart-mode-line-powerline-theme 
                          smex                           ; M-x command completion
                          solarized-theme
                          sunny-day-theme
                          tagedit                        ; HTML tag editing like sexps
                          terraform-mode                 ; Hashicorp's terraform
                          treemacs                       ; Possible replacement for neotree
                          treemacs-evil
                          tronesque-theme
                          web-mode                       ; Web development (html, css, js)
                          white-sand-theme
                          yaml-mode                      ; YAML support
                          zenburn-theme
                          )      
  "Default Packages")

(load "package")
(package-initialize)

;; Set up the package repos we pull from
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; Pin packages to specific repos if we need to
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'(
	  ("deft" . "melpa")
	  ("magit" . "melpa"))))

;; When Emacs boots, check to make sure all the packages defined in ysgard/packages
;; is installed, otherwise install them
(defun ysgard/packages-installed-p ()
  (loop for pkg in ysgard/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (ysgard/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ysgard/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
