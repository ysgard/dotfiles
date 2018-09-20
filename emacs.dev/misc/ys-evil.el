;;; ys-evil.el --- Evil, the only way to work

;;; Commentary:
;;;
;;; EVIL settings unlock the power of Vim!! Bwahahaha

;;; Code:

(use-package evil)
(use-package evil-leader)

(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)

;; Set comma to be leader
(evil-leader/set-leader ",")
(setq evil-leader/in-all-states 1)

;; Turn on evil
(evil-mode 1)

;; Set pretty cursor colors so we get mode-at-a-glance
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; Define initial states for certain modes
;; Some modes, like Dired and the shell,
;; deal very poorly with EVIL
(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
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
                  (haskell-interactive-mode . emacs)
                  (cider-repl-mode . emacs)
                  (treemacs-mode . emacs)
                  (intero-repl-mode . emacs))
      do (evil-set-initial-state mode state))

;; Define ',,' as ESC
(when (require 'key-chord nil 'noerror)
  (key-chord-define evil-normal-state-map ",," 'evil-force-normal-state)
  (key-chord-define evil-visual-state-map ",," 'evil-change-to-previous-state)
  (key-chord-define evil-insert-state-map ",," 'evil-normal-state)
  (key-chord-define evil-replace-state-map ",," 'evil-normal-state))

;; Unset space in normal mode, so we can bind a bunch of useful commands to
;; <space>-<letter>
(define-key evil-motion-state-map " " nil)

(define-key evil-motion-state-map (kbd "SPC b") 'ibuffer)
(define-key evil-motion-state-map (kbd "SPC o") 'other-window)
(define-key evil-motion-state-map (kbd "SPC k") 'kill-current-buffer)
(define-key evil-motion-state-map (kbd "SPC n") 'next-buffer)
(define-key evil-motion-state-map (kbd "SPC p") 'previous-buffer)
(define-key evil-motion-state-map (kbd "SPC s") 'save-buffer)

(provide 'ys-evil)
;;; ys-evil.el ends here
