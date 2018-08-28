;;; -*- lexical-binding: t -*-
;;; core-lib.el --- Core utility functions

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

(require 'core-package)

;; `New Standard Library'
;; f.el    - files and paths  https://github.com/rejeep/f.el
;; s.el    - strings          https://github.com/magnars/s.el
;; dash.el - lists            https://github.com/magnars/dash.el
(use-package f)
(use-package s)
(use-package dash)

(defun core/font-lock-replace-symbol (mode reg sym)
  "Given a major mode MODE, replace the regular expression REG with the symbol SYM when rendering."
  (font-lock-add-keywords
   mode `((,reg
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,sym 'decompose-region)))))))

(defun core/exec (command)
  "Run a shell COMMAND and return its output as a string, whitespace-trimmed."
  (s-trim (shell-command-to-string command)))

(defun core/exec-with-rc (command &rest args)
  "Run a shell COMMAND with ARGS and return a list containing two values: its return code and its whitespace trimmed output."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (s-trim (buffer-string)))))

(defun core/is-exec (command)
  "Return non-nil if COMMAND is an executable on the system search path."
  (f-executable? (s-trim (shell-command-to-string (s-concat "which " command)))))

(defun core/resolve-exec (command)
  "If COMMAND is an executable on the system search path, return its absolute path, or otherwise nil."
  (-let [path (s-trim (shell-command-to-string (s-concat "which " command)))]
    (when (f-executable? path) path)))

(defun core/exec-if-exec (command args)
  "If COMMAND satisfies `core/is-exec', run it with ARGS and return its output as per `core/exec'.  Otherwise, return nil."
  (when (core/is-exec command) (core/exec (s-concat command " " args))))

(defun core/getent (user)
  "Get the /etc/passwd entry for USER as a list of strings, or nil if there is no such user.  Empty fields are nil."
  (-let [ent (core/exec (s-concat "getent passwd " user))]
    (when (not (s-blank? ent))
      (-map (lambda (i) (if (s-blank? i) nil i))
            (s-split ":" ent)))))

(defun core/user-full-name ()
  "Guess the user's full name.  Return nil if no likely name could be found."
  (or (core/exec-if-exec "git" "config --get user.name")
      (elt (core/getent (getenv "USER")) 4)))

(defun core/user-email ()
  "Guess the user's email address.  Return nil if it cannot be found."
  (or (core/exec-if-exec "git" "config --get user.email")
      (getenv "EMAIL")))


(provide 'core-lib)
;;; core-lib.el ends here
