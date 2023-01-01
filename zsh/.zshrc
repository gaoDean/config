sed -n '/---/!p;//q' ~/Documents/todo

autoload -U colors && colors
PS1="%{$fg[grey]%}>%{$reset_color%} "

set -o vi
setopt interactive_comments
/bin/stty discard undef
GPG_TTY=$(tty)
export GPG_TTY

HISTSIZE=1000
SAVEHIST=1000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"
# == bindings ==

bindkey ^S fzf-file-widget

# == functions ==

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[6 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
	# cursor style
		# 0  ⇒  blinking block.
		# 1  ⇒  blinking block (default).
		# 2  ⇒  steady block.
		# 3  ⇒  blinking underline.
		# 4  ⇒  steady underline.
		# 5  ⇒  blinking bar, xterm.
		# 6  ⇒  steady bar, xterm.
    echo -ne "\e[6 q"
}
zle -N zle-line-init
echo -ne '\e[6 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[6 q' ;} # Use beam shape cursor for each new prompt.

# == source ==

# source /usr/local/opt/powerlevel10k/powerlevel10k.zsh-theme
# [[ ! -f ${XDG_CONFIG_HOME:-$HOME/.config}/p10k/p10k.zsh ]] || source ${XDG_CONFIG_HOME:-$HOME/.config}/p10k/p10k.zsh

[ -f ${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf.zsh ] && source ${XDG_CONFIG_HOME:-$HOME/.config}/fzf/fzf.zsh
[ -f /usr/local/share/zsh-you-should-use/you-should-use.plugin.zsh ] && source /usr/local/share/zsh-you-should-use/you-should-use.plugin.zsh
# [ -f $HOME/repos/oth/shellfirm/shell-plugins/shellfirm.plugin.zsh ] && source $HOME/repos/oth/shellfirm/shell-plugins/shellfirm.plugin.zsh


# Load aliases and shortcuts if existent.
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"
zmodload zsh/zprof
zstyle ':completion:*' menu select
# zmodload zsh/complist
autoload -Uz compinit
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C
_comp_options+=(globdots)		# Include hidden files.
