;;;; ~/.emacs.d/init.el --- Summary

;;; All emacs initialization is controlled through here.
;;; Functionality is broken out into subfiles

;;; Commentary:

;;; Code:

;; Bail if we're not using a current version of emacs

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (< emacs-major-version 24)
  (error "Need at least Emacs 24+ for these init scripts to work!"))

;; Base system initialization
(setq user-full-name "Jan Van Uytven")
(setq user-mail-address "ysgard@gmail.com")

;; Set the load path to include scripts in ~/.emacs.d
(add-to-list 'load-path "~/.emacs.d/ysgard")

;; Call the common lisp library (more code to play with, mmmm)
(require 'cl)

;; Call in some common utility functions - these should be function-agnostic, and
;; are used ONLY to augment common functionality, not control packages or emacs
;; behaviour.
(load "ysgard-defuns.el")

;; Load keybinds
(load "ysgard-keybinds.el")

;; Packages
(load "ysgard-packages.el")

;; Base settings (non-external)
(load "ysgard-base.el")

;; Display settings (eye candy!)
(load "ysgard-display.el")

;; Load trivial packages (must be loaded first)
(load "ysgard-misc.el")

;; Load settings for EVIL mode
(when (require 'evil nil 'noerror)
  (load "ysgard-evil.el"))

;; Load Emacs language server packages
;; (load "ysgard-lsp.el")

;; Load settings for Rust
(load "ysgard-rust.el")

;; Load settings for Haskell
(load "ysgard-haskell.el")

;; Load settings for Javascript
(load "ysgard-js.el")

;; Load setting for lisp/clojure
(load "ysgard-lisp.el")

;; Load settings for Clojure
(load "ysgard-clojure.el")

(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "3a69621a68c2d3550a4c777ffc000e1ea66f5bc2f61112814c591e1bda3f5704" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "72c7c8b431179cbcfcea4193234be6a0e6916d04c44405fc87905ae16bed422a" "a25c42c5e2a6a7a3b0331cad124c83406a71bc7e099b60c31dc28a1ff84e8c04" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "889a93331bc657c0f05a04b8665b78b3c94a12ca76771342cee27d6605abcd0e" "ce585b387d84fc4bbb02b8766bfe82607f891e25602ec3550db858e09c10eb7d" "77c65d672b375c1e07383a9a22c9f9fc1dec34c8774fe8e5b21e76dca06d3b09" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "b747fb36e99bc7f497248eafd6e32b45613ee086da74d1d92a8da59d37b9a829" "0f97285f9e0c7d9cad04f2130859d20d6c9b3142877b2bca52d958f4f1cf346f" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6bb466c89b7e3eedc1f19f5a0cfa53be9baf6077f4d4a6f9b5d087f0231de9c8" "cedd3b4295ac0a41ef48376e16b4745c25fa8e7b4f706173083f16d5792bb379" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (yaml-mode web-mode terraform-mode smex smart-mode-line-powerline-theme smart-mode-line rainbow-delimiters racer py-autopep8 perspective org-bullets neotree multi-term monokai-theme mode-icons markdown-mode magit linum-relative leuven-theme key-chord js2-mode intero groovy-mode google-this google-maps flycheck-rust flycheck fireplace exec-path-from-shell evil-leader evil yasnippet ein dtrt-indent dockerfile-mode docker dired+ cyberpunk-theme color-theme-sanityinc-tomorrow cargo)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(tramp-syntax (quote default) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
