class Dotfiles < Thor
  include Thor::Actions
  Thor::Sandbox::Dotfiles.source_root(File.expand_path('..', __FILE__))
  @user = %x[whoami].chomp

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
  def install_nvim
    link_file("#{Dir.pwd}/nvim/init.vim", "~#{@user}/.config/nvim/init.vim", options[:force])
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
      if run('which apt').include?('apt not found')
        run 'yum install -y fish'
      else
        run 'apt install -y fish'
      end
    else
      raise "Cannot figure out how to install fish, unsupported OS: #{RUBY_PLATFORM}"
    end
    # Install oh-my-fish
    run 'curl -L https://get.oh-my-fish | fish'
    # Install the powerline fonts
    thor :install_powerline_fonts
    # Install bobthefish
    run 'omf install bobthefish'
  end
end

