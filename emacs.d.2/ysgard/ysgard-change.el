;;; ysgard-change.el --- Summary
;;; Change.org specific stuff

;;; Commentary:

;;; Utility functions and routines to help Change.org workflows

;;; Code:

(defvar scratch-pad-location (expand-file-name "~/Notes/scratch_pad.org"))

;; Open scratch pad

(global-set-key (kbd "C-l s") (find-file scratch-pad-location))


(provide 'ysgard-change)
;;; ysgard-change.el ends here
                
