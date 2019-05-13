;;; ys-helpers.el --- Summary

;;; Helper Functions

;;; Commentary:

;;; Various functions used to configure emacs

;; system-types:
;;   gnu/linux     - linux
;;   darwin        - os x
;;   berkeley-unix - FreeBSD
;;   ms-dos        - MS-DOS
;;   windows-nt    - Win32

;;; Code:

(require 'cl)

(defun ys-online? ()
  "Returns (up) if we have a network connection, nil otherwise"
  (if (and (functionp 'network-interface-list)
	   (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
			      (member 'up (first (last (network-interface-info
							(car iface)))))))
	    (network-interface-list))
    t))


(provide 'ys-helpers)
;;; ys-helpers.el ends here
