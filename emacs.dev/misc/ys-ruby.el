;;; ys-ruby.el --- Ruby and Chef config

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Berksfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("Thorfile\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode)
         ("\\.vagrant\\'" . ruby-mode))
  :interpreter "ruby"
  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)
  :bind (([(meta down)] . ruby-forward-sexp)
         ([(meta up)] . ruby-backward-sexp)
         ("C-c C-e" . ruby-send-region)))

(use-package rubocop
  :hook (ruby-mode . rubocop-mode)
  :diminish rubocop-mode)

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode))

(provide 'ys-ruby)
;;; ys-ruby.el ends here
