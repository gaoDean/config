sed -n '/---/!p;//q' ~/dox/todo

RED="09"
GREEN="0a"
YELLOW="0b"
BLUE="0c"
MAGENTA="0d"
CYAN="0e"
WHITE="0f"
BLK="00" CHR="00" DIR="0e" EXE="0b" REG="00" HARDLINK="0e" LINK="0a" MISSING="00" ORPHAN="09" FIFO="0e" SOCK="00" OTHER="05"
DIR=$BLUE
EXE=$GREEN
SOCK=$MAGENTA
FIFO=$YELLOW
LINK=$CYAN

set -o vi

# == config ==

export EDITOR=nvim
export PATH="/Library/Frameworks/Python.framework/Versions/3.8/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

# == plugin config ==

export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$LINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"
export NNN_READER=preview
export NNN_OPENER=nuke
export NNN_TRASH=2

GIT_FILES='git rev-parse --show-toplevel 2> /dev/null'
export FZF_DEFAULT_COMMAND="fd . -t f --hidden --follow --exclude .git --strip-cwd-prefix"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd . -t d --follow --strip-cwd-prefix"
export FZF_DEFAULT_OPTS='--border'
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
--color=fg:7,hl:11
--color=fg+:15,bg+:0,hl+:3
--color=info:14,prompt:12,pointer:11
--color=marker:4,spinner:#73d0ff,header:#d4bfff'

# == bindings ==
bindkey ^S fzf-file-widget

# == source ==

source /usr/local/opt/powerlevel10k/powerlevel10k.zsh-theme
[[ ! -f ${XDG_CONFIG_HOME:-$HOME/.config}/p10k/p10k.zsh ]] || source ${XDG_CONFIG_HOME:-$HOME/.config}/p10k/p10k.zsh

[ -f ${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf.zsh ] && source ${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf.zsh

# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"

zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}'
zstyle :compinstall filename '/Users/deangao/.zshrc'
autoload -Uz compinit && compinit
