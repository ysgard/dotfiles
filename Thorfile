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
    empty_directory "#{Dir.pwd}/vim/bundle"
    run "#{Dir.pwd}/vim/update.sh"
    link_file("#{Dir.pwd}/vim", "~#{@user}/.vim", options[:force])
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
    remove_file "~#{Dir.pwd}/vim/bundle"
  end
end

