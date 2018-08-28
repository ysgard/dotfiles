;;; -*- lexical-bunding: t -*-
;;; init.el --- A beginning is a very delicate time...

;; Copyright (C) 2018 Jan Van Uytven

;; Author: Jan Van Uytven <ysgard@gmail.com>

;; This code is somewhat based on Bodil Stokke's 'ohai-emacs'
;; dotfile configuration, with many modifications for my own use.
;; The original configuration can be found here, and any
;; incidental brilliance you will find in this mess is due
;; to her code, not mine:
;; https://github.com/bodil/ohai-emacs

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

;; We only run on Emacs 24.4 or higher
(when (or (< emacs-major-version 24)
          (and (= emacs-major-version 24) (< emacs-minor-version 4)))
  (x-popup-dialog
   t `(,(format "Sorry, you need GNU Emacs version 24.4 or higher.

Your installed Emacs reports:
%s" (emacs-version))
       ("OK :(" . t)))
  (save-buffers-kill-emacs t))

;; Initialize the package system
(package-initialize)

;; No splash screen
(setq inhibit-startup-message t)

;; Set the hostname
(setq hostname (replace-regexp-in-string
                "\\(^[[:space:]\n]*\\|[[:space]\n]*$\\)" ""
                (with-output-to-string
                  (call-process "hostname" nil standard-output))))

;; Set the dotfiles dir, found by getting the path to init.el
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))

;; Load the library packages in corelib
(add-to-list 'load-path (concat dotfiles-dir "corelib"))

;; Load the configuration modules
(add-to-list 'load-path (concat dotfiles-dir "modules"))

;; Define location of autoloads and custom definitions
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Load any custom settings
(load custom-file 'noerror)

;; Load the corelibs
(require 'core-lib)
(require 'core-package)

;; Load the enabled modules

;; Load the user's config, if it exists
(load (concat dotfiles-dir "user.el") 'noerror)
