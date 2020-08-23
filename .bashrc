shopt -s nocaseglob #case insensitive completion

[ -f /etc/bash_completion ] && . /etc/bash_completion
[[ $- =~ i ]] && stty -ixoff -ixon # Disable CTRL-S and CTRL-Q

export EDITOR=vim
# this causes issues when using clojure, it defaults the .clojure to .config/clojure
# export XDG_CONFIG_HOME=$HOME/.config # should only be set for linux..
export PAGER='less -S'
export SSH_AUTH_SOCK=$HOME/.ssh/ssh-agent.socket
export PATH="/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/local/opt/node@12/bin:$HOME/.config/npm/bin:$HOME/.local/bin:/usr/bin:/usr/sbin:$HOME/go/bin"
export CDPATH=$HOME/src
export NPM_CONFIG_PREFIX=$HOME/.config/npm
export GEM_HOME=$HOME/.config/gems

alias gr='cd $(git rev-parse --show-toplevel || echo ".")'
alias ..='cd ..'
alias t='tmux attach -t vty || tmux new -s vty'
alias de='export $(egrep -v "^#" .env | xargs)'
alias xclip='xclip -sel clip'
alias jv="jq -C | less -R"
alias ssha="ssh-agent -a $SSH_AUTH_SOCK && ssh-add ~/.ssh/id_rsa"
# alias open='xdg-open' # only for linux
alias vi='vim'
alias get-music='youtube-dl --extract-audio --audio-format m4a'
alias gifify='ffmpeg -i $1 -s 600x400 -pix_fmt rgb24 -r 10 -f gif - | gifsicle --optimize=3 --delay=3 > out.gif'
alias rkb='xset r rate 200 25 && setxkbmap -layout us -option ctrl:nocaps'
alias pg='pg_ctl -D /usr/local/var/postgres' # start/stop
viw() { vi `which "$1"`; }
pp() { until ping -c1 1.1.1.1 >/dev/null 2>&1; do :; done; }
git_branch() { echo -e "$(git branch 2>/dev/null| sed -n '/^\*/s/^\* //p')"; }

git_state() {
    if git rev-parse --git-dir >/dev/null 2>&1; then
        echo -ne "$(git_branch)"
    else
        echo -ne "!"
    fi
}

jobs_marker() {
  local n=$(jobs | wc -l)
  ((n)) && echo -n '&' || echo -n '$'
}

PROMPT_COMMAND='PS1="\W($(git_state)) $(jobs_marker) "'

# [ -f ~/.fzf.bash ] && source ~/.fzf.bash
# source /usr/local/share/chruby/chruby.sh
# chruby 2.7.1
