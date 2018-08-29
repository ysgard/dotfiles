;;; -*- lexical-binding: t -*-
;;; core-set-path.el --- Set the executable search path from the user shell

;; Copyright (C) 2018 Jan Van Uytven
;; Author: Jan Van Uytven <ysgard@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; To make sure we have full access to the system search path, import
;; it using the `exec-path-from-shell' package.

(paradox-require 'exec-path-from-shell)

(when (memq window-system '(x max ns))
  (exec-path-from-shell-initialize))

(provide 'core-set-path)
;;; core-set-path.el ends here
