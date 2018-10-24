class Dot < Thor
  include Thor::Actions
  Thor::Sandbox::Dot.source_root(File.expand_path('..', __FILE__))
  @user = %x[whoami].chomp
  @ruby_version = "2.5.1"
  @@dont_install = %w[
                Gemfile
                Gemfile.lock
                Thorfile
                README.md
                LICENSE.md
                fish
                inputrc-osx
                vim
                nvim
                emacs.d
                emacs.dev
                emacs.d.old
                tmux.conf
                zshrc
                zshenv
                ]



  desc "install", "Install all dotfiles into #{@user}'s home directory"
  method_options :force => :boolean
  def install
    Dir['*'].each do |file|
      next if @@dont_install.include?(file)
      copy_file(file, "~#{@user}/.#{file}", options[:force])
    end
    if RUBY_PLATFORM.include?('darwin')
      link_file("#{Dir.pwd}/inputrc-osx", "~#{@user}/.inputrc", options[:force])
    end
    empty_directory "~#{@user}/tmp"
  end

  desc "install_vim", "Install vim config files into #{@user}'s home directory"
  method_options :force => :boolean
  def install_vim
    empty_directory "~#{@user}/.vim/bundle"
    empty_directory "~#{@user}/.vim/autoload"
    empty_directory "~#{@user}/.vim/colors"
    link_file("#{Dir.pwd}/vim/vimrc", "~#{@user}/.vim/vimrc", options[:force])
    copy_file("#{Dir.pwd}/vim/update.sh", "~#{@user}/.vim/update.sh", options[:force]) 
    chmod "~#{@user}/.vim/update.sh", 0755
    run "curl -LSso ~#{@user}/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim"
    run "curl -LSso ~#{@user}/.vim/colors/molokai.vim https://raw.githubusercontent.com/tomasr/molokai/master/colors/molokai.vim"
    inside("~#{@user}/.vim") do
      run "~#{@user}/.vim/update.sh"
    end
  end

  desc "install_clozure", "Installs Clozure Common Lisp (CCL) and Quicklisp, putting it into ~/ccl and ~/quicklisp, and install slime-helper."
  method_option :clean,
                :type => :boolean,
                :default => false,
                :desc => "Remove existing config"
  def install_clozure
    version = "1.11.5"
    base_download_url = "https://github.com/Clozure/ccl/releases/download/v#{version}/ccl-#{version}"
    ql_install = "(progn (quicklisp-quickstart:install :path \"~#{@user}/quicklisp\") (ccl::quit))"
    if options[:clean]
      remove_file "~#{@user}/ccl"
      remove_file "~#{@user}/quicklisp"
      remove_file "~#{@user}/bin/ccl"
      remove_file "~#{@user}/.ccl-init.lisp"
    end
    empty_directory "~#{@user}/bin"
    # Install Clozure
    if RUBY_PLATFORM.include?('darwin')
      run "curl -L -o ~#{@user}/ccl.tar.gz #{base_download_url}-darwinx86.tar.gz"
    else
      run "curl -L -o ~#{@user}/ccl.tar.gz #{base_download_url}-linuxx86.tar.gz"
    end
    inside("~#{@user}") { run "tar zxvf ~#{@user}/ccl.tar.gz" }
    if RUBY_PLATFORM.include?('darwin')
      link_file("~#{@user}/ccl/dx86cl64", "~#{@user}/bin/ccl", :force)
    else
      link_file("~#{@user}/ccl/lx86cl64", "~#{@user}/bin/ccl", :force)
    end
    remove_file "~#{@user}/ccl.tar.gz"
    # Install Quicklisp
    run "curl -L -o ~#{@user}/ccl/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp"
    inside("~#{@user}/ccl") do
      run "~#{@user}/bin/ccl -l quicklisp.lisp -e '#{ql_install}'"
      run "~#{@user}/bin/ccl -l ~#{@user}/quicklisp/setup.lisp -e '(progn (ql:add-to-init-file) (ccl::quit))'"
      run "~#{@user}/bin/ccl -l ~#{@user}/quicklisp/setup.lisp -e \
          '(progn (ql:quickload \"quicklisp-slime-helper\") (ccl::quit))'"
    end
  end

  desc "install_tmux", "Installs tmux config files into #{@user}'s home dir, and optionally plugin manager and plugins"
  method_option :force,
                 :type => :boolean,
                 :default => false,
                 :desc => "Don't prompt when overwriting"
  method_option :everything,
                 :type => :boolean,
                 :default => false,
                 :desc => "Install tmux plugins and manager"
  method_option :clean,
                 :type => :boolean,
                 :default => false,
                 :desc => "Remove existing config"
  def install_tmux
    if options[:clean]
      remove_file "~#{@user}/.tmux.conf"
      remove_file "~#{@user}/.tmux"
    end
    if options[:everything]
      run "git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm"
    end
    copy_file("#{Dir.pwd}/tmux.conf", "~#{@user}/.tmux.conf", options[:force])
  end
      
  desc "install_zsh", "Install zsh config file, oh-my-zsh, and plugins into #{@user}'s home directory"
  method_option :force,
    :type => :boolean,
    :default => false,
    :desc => "Overwrite existing files"
  method_option :clean,
    :type => :boolean,
    :default => false,
    :desc => "Remove zsh configuration"
  method_option :everything,
    :type => :boolean,
    :default => false,
    :desc => "Install oh-my-zsh and plugins too"
  def install_zsh
    if options[:clean]
      remove_file "~#{@user}/.zshrc"
      remove_file "~#{@user}/.zshenv"
      remove_file "~#{@user}/.oh-my-zsh"
      remove_file "~#{@user}/.zsh"
    end
    if options[:everything]
      run "curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | bash"
      run "git clone https://github.com/zsh-users/zsh-autosuggestions ~#{@user}/.zsh/zsh-autosuggestions"
      run "git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~#{@user}/.zsh/zsh-syntax-highlighting"
    end
    copy_file("#{Dir.pwd}/zshrc", "~#{@user}/.zshrc", options[:force])
    copy_file("#{Dir.pwd}/zshenv", "~#{@user}/.zshenv", options[:force])
  end

  desc "install_nvim", "Install nvim config files into #{@user}'s home directory"
  method_options :force => :boolean
  method_option :filesonly, :aliases => '-f', :desc => "Only config files, no pip3 or dein"
  def install_nvim
    if !options[:filesonly]
      url = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
      run "curl -fLo ~#{@user}/.local/share/nvim/site/autoload/plug.vim --create-dirs #{url}"
    end
    link_file("#{Dir.pwd}/nvim/init.vim", "~#{@user}/.config/nvim/init.vim", options[:force])
    Dir['nvim/config/*'].each do |config_file|
      link_file(config_file, "~#{@user}/.config/#{config_file}", options[:force])
    end
  end

  desc "clean_all", "Remove all dotfile links from the #{@user}'s home directory"
  method_options :force => :boolean
  def clean_all
    Dir['*'].each do |file|
      next if @@dont_install.include?(file)
      remove_file "~#{@user}/.#{file}"
    end
    remove_file "~#{@user}/.config/fish/config.fish"
    remove_file "~#{@user}/.config/fish/functions"
    remove_file "~#{@user}/.vim"
    remove_file "~#{@user}/.config/nvim"
    remove_file "~#{@user}/.cache/dein"
    remove_file "~#{@user}/.cache/plugged"
  end

  desc "install_powerline_fonts", "Install fonts patched for Powerline"
  method_options :force => :boolean
  def install_powerline_fonts
    empty_directory "~#{@user}/src"
    run "git clone https://github.com/powerline/fonts.git --depth=1 ~#{@user}/src/powerline_fonts"
    inside("~#{@user}/src/powerline_fonts") do
      run './install.sh'
    end
  end

  desc "install_nerd_fonts", "Install the Nerd fonts (ryanoasis/nerd-fonts)"
  method_options :force => :boolean
  def install_nerd_fonts
    empty_directory "~#{@user}/src"
    run "git clone https://github.com/ryanoasis/nerd-fonts --depth=1 ~#{@user}/src/nerd-fonts"
    inside("~#{@user}/src/nerd-fonts") do
      run './install.sh'
    end
  end

  desc "install_fish", "Install fish, oh-my-fish, and bobthefish"
  method_options :force => :boolean
  def install_fish
    # Install fish
    if RUBY_PLATFORM.include?('darwin')
      run 'brew install fish'
    elsif RUBY_PLATFORM.include?('linux-gnu')
      if run('which apt')
        run 'sudo apt install -y fish'
      else
        run 'sudo yum install -y fish'
      end
    else
      raise "Cannot figure out how to install fish, unsupported OS: #{RUBY_PLATFORM}"
    end
    # Install oh-my-fish
    remove_file "~#{@user}/.local/share/omf"
    run 'curl -L https://get.oh-my.fish | fish'
    # Install the powerline fonts
    self.install_powerline_fonts
    # Install bobthefish
    run 'omf install bobthefish'
  end

  desc "prep_ubuntu", "Installs prerequisites for ubuntu"
  def prep_ubuntu
    run 'sudo apt install -y wget vim neovim emacs curl ruby python3 python3-pip'
    run 'sudo gem install bundler thor'
    run 'sudo apt install -y libssl-dev libreadline-dev'
  end

  desc "install_rbenv", "Installs rbenv and ruby_build"
  method_option :version, 
    :default => @ruby_version, 
    :alias => "-v",
    :desc => "The version to pass to ruby-build"
  def install_rbenv
    run "git clone https://github.com/rbenv/rbenv.git ~#{@user}/.rbenv"
    run "~#{@user}/.rbenv/bin/rbenv init"
    empty_directory "~#{@user}/.rbenv/plugins"
    run "git clone https://github.com/rbenv/ruby-build.git ~#{@user}/.rbenv/plugins/ruby-build"
    run "~#{@user}/.rbenv/bin/rbenv rehash"
    run "~#{@user}/.rbenv/bin/rbenv install #{options[:version]}"
  end

  desc 'install_rust', 'Installs rust & cargo via rustup'
  def install_rust
    run 'curl https://sh.rustup.rs -sSf | sh'
    run 'rustup toolchain install nightly'
    run 'rustup default nightly'
    run 'rustup component add rust-src'
    run 'cargo install racer'
  end

  # Install development emacs environment
  desc 'install_emacs_d', 'Installs emacs.d into ~/.emacs.d'
  method_option :force,
                :type => :boolean,
                :default => false,
                :desc => 'Overwrite existing links'
  method_option :clean,
                :type => :boolean,
                :default => false,
                :desc => 'Delete old .emacs.dev'
  def install_emacs_d
    if options[:clean]
      remove_file "~#{@user}/.emacs.d"
    end
    empty_directory "~#{@user}/.emacs.d"
    Dir['emacs.d/*.el'].each do |file|
      copy_file("#{file}", "~#{@user}/.#{file}", options[:force])
    end
    Dir['emacs.d/ys/*.el'].each do |file|
      f = file.split('/')[1..-1].join('/')
      copy_file("#{file}", "~#{@user}/.emacs.d/#{f}", options[:force])
    end
    Dir['emacs.d/misc/*.el'].each do |file|
      f = file.split('/')[1..-1].join('/')
      copy_file("#{file}", "~#{@user}/.emacs.d/#{f}", options[:force])
    end
    empty_directory "~#{@user}/.emacs.d/thirdparty"
    run "git clone https://github.com/aurelienbottazini/tronesque ~#{@user}/.emacs.d/thirdparty/tronesque"
    link_file("~#{@user}/.emacs.d/thirdparty/tronesque/themes/tronesque-theme.el", "~#{@user}/.emacs.d/tronesque-theme.el")
  end

  desc 'toggle_emacs_dev', 'backup .emacs.d and install a link to .emacs.dev'
  def toggle_emacs_dev
    # If .emacs.bak exists, a dev env is in place, replace it
    if Dir.exist?(File.expand_path("~#{@user}/.emacs.bak"))
      remove_file "~#{@user}/.emacs.d"
      run "mv ~#{@user}/.emacs.bak ~#{@user}/.emacs.d"
    else
      run "mv ~#{@user}/.emacs.d ~#{@user}/.emacs.bak"
      self.install_emacs_dev
      link_file("~#{@user}/.emacs.dev", "~#{@user}/.emacs.d")
    end
  end
end

