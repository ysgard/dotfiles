;;; ys-lua.org --- Lua mode and configuration

;;; Commentary:

;;; Code:

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package flymake-lua
  :after (lua)
  :hook (lua-mode . flymake-lua-load))

(provide 'ys-lua)
;;; ys-lua.el ends here
