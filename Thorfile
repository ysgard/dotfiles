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
                emacs.new
                ]



  desc "install", "Install all dotfiles into #{@user}'s home directory"
  method_options :force => :boolean
  def install
    Dir['*'].each do |file|
      next if @@dont_install.include?(file)
      link_file(file, "~#{@user}/.#{file}", options[:force])
    end
    link_file("#{Dir.pwd}/fish/config.fish", "~#{@user}/.config/fish/config.fish", options[:force])
    link_file("#{Dir.pwd}/fish/functions", "~#{@user}/.config/fish/functions", options[:force])
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

  desc 'install_emacs', 'Installs emacs config into ~/.emacs.d'
  method_option :force,
    :type => :boolean,
    :default => false,
    :desc => "Overwrite existing links"
  method_option :clean,
    :type => :boolean,
    :default => false,
    :desc => "Remove old .emacs.d"
  def install_emacs
    if options[:clean]
      remove_file "~#{@user}/.emacs.d"
    end
    empty_directory "~#{@user}/.emacs.d"
    copy_file("#{Dir.pwd}/emacs.d/init.el", "~#{@user}/.emacs.d/init.el",
              options[:force])
    Dir['emacs.d/ysgard/*'].each do |file|
      f = file.split('/')[1..-1].join('/')
      copy_file("#{file}",
                "~#{@user}/.emacs.d/#{f}",
                options[:force])
    end
    # Install custom packages
    run "git clone https://github.com/emacs-lsp/lsp-mode" \
        " ~#{@user}/.emacs.d/lsp-mode"
    run "git clone https://github.com/emacs-lsp/lsp-ui" \
        " ~#{@user}/.emacs.d/lsp-ui"
    run "git clone https://github.com/emacs-lsp/lsp-rust" \
        " ~#{@user}/.emacs.d/lsp-rust"
  end

  # Install development emacs environment
  desc 'install_emacs_dev', 'Installs emacs.dev into ~/.emacs.dev'
  method_option :force,
                :type => :boolean,
                :default => false,
                :desc => 'Overwrite existing links'
  method_option :clean,
                :type => :boolean,
                :default => false,
                :desc => 'Delete old .emacs.dev'
  def install_emacs_dev
    if options[:clean]
      remove_file "~#{@user}/.emacs.dev"
    end
    empty_directory "~#{@user}/.emacs.dev"
    Dir['emacs.dev/*.el'].each do |file|
      copy_file("#{file}", "~#{@user}/.#{file}", options[:force])
    end
    Dir['emacs.dev/ys/*.el'].each do |file|
      f = file.split('/')[1..-1].join('/')
      copy_file("#{file}", "~#{@user}/.emacs.dev/#{f}", options[:force])
    end
    Dir['emacs.dev/misc/*.el'].each do |file|
      f = file.split('/')[1..-1].join('/')
      copy_file("#{file}", "~#{@user}/.emacs.dev/#{f}", options[:force])
    end
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

