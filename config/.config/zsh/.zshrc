[[ `whoami` == root ]] && source /root/.config/zsh/.zshrc
export PATH="$PATH:$HOME/.scripts/:$HOME/.scripts/web:$HOME/.scripts/status-modules:$HOME/.cargo/bin"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
export EDITOR="/usr/bin/nvim"
GPG_TTY=$(tty)
export GPG_TTY
#export $(dbus-launch)
# If not running interactively, don't do anything
[[ $- != *i* ]] && return
# Hisotry in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Basic auto/tab complete
autoload -U compinit promptinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)
promptinit; 

#Vi Mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Edit line in vim with ctrl-e
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

[ -f "$HOME/.config/zsh/aliasrc" ] && source "$HOME/.config/zsh/aliasrc"

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}
lfcd () {
tmp="$(mktemp)"
lf -last-dir-path="$tmp" "$@"
if [ -f "$tmp" ]; then
	dir="$(cat "$tmp")"
	rm -f "$tmp" > /dev/null
	[ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
fi
}
bindkey -s '^o' 'lfcd\n'
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-you-should-use/you-should-use.plugin.zsh
[[ $TERM == eterm-color ]] && screen && exit || eval "$(/usr/bin/starship init zsh)"
