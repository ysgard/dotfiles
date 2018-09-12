export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"
plugins=(aws brew bundler cargo docker docker-compose emacs gem git git-extras github gitignore kitchen knife knife_ssh npm osx rails rake rbenv ruby rust screen stack sudo yarn)

source $ZSH/oh-my-zsh.sh
eval "$(rbenv init -)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
	source /etc/profile.d/vte.sh
fi
