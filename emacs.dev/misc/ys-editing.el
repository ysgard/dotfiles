;;; ys-editing.el --- generic editing functions and helpers.

;;; Commentary:

;;; A couple of useful functions and bindings to help editing code/text.

;;; Code:

;; Multiple Cursors
;; Use <insert> to place a cursor on the next match for the current selection.
;; Use S-<insert> to place on on the previous match.
;; Use C-' to use extended mark mode (more control)
;; Use C-" to place cursors on all matches
;; Select a region and use C-M-' to place cursors on each line
;; (use-package multiple-cursors
;;   :commands multiple-cursors-mode
;;   :config
;;   ;; clear existing binding for C-'
;;   (bind-keys :map mc/keymap
;;              ("C-'" . nil))
;;   :bind (("<insert>" . mc/mark-next-like-this)
;;          ("C-'" . mc/mark-more-like-this-extended)
;;          ("C-\"" . mc/mark-all-like-this-dwim)
;;          ("C-M-'" . mc/edit-lines)))

;; Use C-= to select the innermost logical unit.
;; Keep hitting C-= to expand to next logical.
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; Remap join-line to M-j
;; join-line joins the line to the line above in a
;; logical way for the file you're editing.
(global-set-key (kbd "M-j") 'join-line)

;; Automatically insert matching braces and whatnot.
(electric-pair-mode 1)

;; Duplicate start of line or region with C-M-<end>
;; From http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))

(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))

(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(global-set-key (kbd "C-M-<end>") 'duplicate-start-of-line-or-region)

;; Hack for setting a fixed wrap column in visual-line-mode
;; From https://www.emacswiki.org/emacs/VisualLineMode
(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap as NEW-WRAP-COLUMN in BUFFER (defaults to current buffer)
   by setting the right-hand margin on every window that displays BUFFER.  A value
   of NIL or 0 for NEW-WRAP-COLUMN disables this behaviour."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-marginx nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column))))))

;; A function for easily editing a file as root through TRAMP
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (if (fboundp 'helm-read-file-name)
                             (helm-read-file-name "File: ")
                           (ido-read-file-name "File: "))))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; A key for intelligently shrinking whitespace
;; From https://github.com/jcpetkovich/shrink-whitespace.el
(use-package shrink-whitespace
  :commands shrink-whitespace
  :bind ("C-c DEL" . shrink-whitespace))

;; Highlight changed areas with certain operations, such as undo, kill or yank
(use-package volatile-highlights
  :commands volatile-highlights-mode
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before 'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)
  (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

(provide 'ys-editing)
;;; ys-editing.el ends here
