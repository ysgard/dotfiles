export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"
plugins=(aws bundler cargo docker docker-compose emacs gem git git-extras github gitignore kitchen knife knife_ssh kubectl npm osx rails rake rbenv ruby rust screen terraform sudo yarn)

source $ZSH/oh-my-zsh.sh
eval "$(rbenv init -)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
	source /etc/profile.d/vte.sh
fi

# Autocomplete
if [ -f ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
	source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

# Syntax highlighting (must be last)
if [ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
	source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

if [ -x "$(command -v nvim)" ]; then
  alias vim=nvim
fi

if [ -f ~/.zshrc-local ]; then
  source ~/.zshrc-local
fi
