;;; ys-json.el --- JSON config

;;; Commentary:

;;; Loaded by a couple of other modules

;;; Code:

(use-package json-mode
  :commands json-mode
  :config
  (bind-keys :map json-mode-map
             ("C-c <tab>" . json-mode-beautify)))

(provide 'ys-json)
;;; ys-json.el ends here
