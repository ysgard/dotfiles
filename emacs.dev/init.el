;;;; ~/.emacs.d/init.el --- Summary

;;; All emacs initialization is controlled through here.
;;; Functionality is broken out into subfiles

;;; Commentary:

;;; Code:

;; Bail if we're not using a current version of emacs
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
;;(require 'ys-rust)
(require 'ys-lisp)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme yaml-mode white-sand-theme web-mode tronesque-theme treemacs-evil treemacs terraform-mode tagedit sunny-day-theme solarized-theme smex smart-mode-line-powerline-theme smart-mode-line soft-stone-theme silkworm-theme rainbow-delimiters racer professional-theme perspective org-bullets oldlace-theme occidental-theme nord-theme multi-term molokai-theme monokai-theme moe-theme mode-icons material-theme magit linum-relative leuven-theme kibit-helper key-chord js2-mode intero ido-vertical-mode ido-completing-read+ hydra groovy-mode google-this google-maps github-theme ghc flycheck-rust flycheck flx-ido fireplace exec-path-from-shell evil-leader evil dockerfile-mode docker dtrt-indent d-mode cyberpunk-theme cql-mode company-lsp company-ghci company color-theme-sanityinc-tomorrow color-theme-modern clojure-mode-extra-font-locking cider cargo blackboard-theme auto-complete alect-themes abyss-theme)))
 '(tramp-syntax (quote default) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
