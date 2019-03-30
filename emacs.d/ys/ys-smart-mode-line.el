;;; ys-smart-mode-line.el --- A cleaner modeline

;;; Commentary:

;;; Code:

(use-package "rich-minority"
  :commands rich-minority-mode
  :demand t
  :init
  (setq rm-blacklist '(" Helm" " Guide" " $" " ," " Tern" " Ind" " alchemist" " Monroe" " cljr" " Wrap" " Doc"))
  :config
  (rich-minority-mode 1))

(use-package "smart-mode-line"
  :commands sml/setup
  :demand t
  :init
  (setq sml/theme 'respectful
        sml/shorten-directory t
        sml/name-width 40
        sml/mode-width 'full)
  :config
  (sml/setup))

(provide 'ys-smart-mode-line)
;;; ys-smart-mode-line.el ends here
