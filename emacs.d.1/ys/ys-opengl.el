;;; ys-opengl.el --- Configuration for working with OpenGL files

;;; Commentary:

;;; Code:

(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)))

(provide 'ys-opengl)
;;; ys-opengl.el ends here
