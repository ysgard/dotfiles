;;; -*- lexical-binding: t -*-
;;; core-package.el --- Package system configuration

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

;; Function that checks if we are online.
(require 'cl)
(defun online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                              (member 'up (first (last (network-interface-info
                                                        (car iface)))))))
            (network-interface-list))
    t))

;; Use both Elpa and Melpa for the origins of packages
(setq package-used-dir (concat dotfiles-dir "elpa"))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Initialize, and start downloading packages if we're online
(package-initialize)
(when (online?)
  (unless package-archive-contents (package-refresh-contents)))

;; Install paradox, a superior interface for package management
(when (not (package-installed-p 'paradox))
  (package-install 'paradox))

;; use-package is how we're going to manage our function dependencies,
;; load orders, config options, etc...
(paradox-require 'use-package)
(require 'use-package)

;; Automatically install packages defined by use-package
(setq use-package-always-ensure t)

(provide 'core-package)
;;; core-package ends here
