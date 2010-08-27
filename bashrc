#!/bin/bash
#
# Mitchell Hashimoto's bash environment
# Much taken from Ryan Tomayko (thanks!)

# Basics
: ${HOME=~}
: ${LOGNAME=$(id -un)}
: ${UNAME=$(uname)}

# Complete hostnames from this file
: ${HOSTFILE=~/.ssh/known_hosts}

#-------------------------------------------------------------------------------
# Shell Options
#-------------------------------------------------------------------------------

# System bashrc
test -r /etc/bash.bashrc && . /etc/bash.bashrc

# Notify bg task completion immediately
set -o notify

# Fucking mail notifications
unset MAILCHECK

# default umask
umask 0022

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------

# Various sbins
PATH="/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin"
PATH="/usr/local/bin:$PATH"

# Append macports bin (/opt) if on mac
if [[ `uname` == "Darwin" ]]; then
    PATH="/opt/local/bin:$PATH"
    PATH="/opt/local/sbin:$PATH"
    MANPATH="/opt/local/share/man:$MANPATH"
fi

# ~/bin if it exists
test -d "$HOME/bin" &&
PATH="$HOME/bin:$PATH"

#-------------------------------------------------------------------------------
# Env. Configuration
#-------------------------------------------------------------------------------

# detect interactive shell
case "$-" in
    *i*) INTERACTIVE=yes ;;
    *)   unset INTERACTIVE ;;
esac

# detect login shell
case "$0" in
    -*) LOGIN=yes ;;
    *)  unset LOGIN ;;
esac

# Proper locale
: ${LANG:="en_US.UTF-8"}
: ${LANGUAGE:="en"}
: ${LC_CTYPE:="en_US.UTF-8"}
: ${LC_ALL:="en_US.UTF-8"}
export LANG LANGUAGE LC_CTYPE LC_ALL

# Always use passive mode FTP
: ${FTP_PASSIVE:=1}
export FTP_PASSIVE

# Ignore backups, CVS directories
FIGNORE="~:CVS:#:.pyc"
HISTCONTROL=ignoreboth

#-------------------------------------------------------------------------------
# Editor and Pager
#-------------------------------------------------------------------------------
EDITOR=emacs
export EDITOR

PAGER="less -FirSwX"
MANPAGER="$PAGER"
export PAGER MANPAGER

#-------------------------------------------------------------------------------
# Prompt
#-------------------------------------------------------------------------------
RED="\[\033[0;31m\]"
BROWN="\[\033[0;33m\]"
GREY="\[\033[0;97m\]"
GREEN="\[\033[0;32m\]"
BLUE="\[\033[0;34m\]"
PS_CLEAR="\[\033[0m\]"
SCREEN_ESC="\[\033k\033\134\]"

COLOR1="${BLUE}"
COLOR2="${BLUE}"
P="\$"

prompt_simple() {
    unset PROMPT_COMMAND
    PS1="\W\$(parse_git_branch) → "
    PS2="> "
}

prompt_compact() {
    unset PROMPT_COMMAND
    PS1="${COLOR1}${P}${PS_CLEAR} "
    PS2="> "
}

prompt_color() {
    PS1="${GREEN}\W\$(parse_git_branch) → ${GREY}"
    PS2="\[[33;1m\]continue \[[0m[1m\]> "
}

parse_git_branch() {
    [ -d .git ] || return 1
    git name-rev HEAD 2> /dev/null | awk "{print \$2 }" | sed 's#\(.*\)#\[\1\]#'
}

#-------------------------------------------------------------------------------
# Aliases / Functions
#-------------------------------------------------------------------------------
alias dul='du -h --max-depth=1'
alias hi='history | tail -20'
alias ack='ack-grep'

# Git aliases
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git log'
alias gcp='git cherry-pick'
alias gco='git checkout'

# Others
alias v='vagrant'

# Usage: puniq [path]
# Remove duplicate entries from a PATH style value while
# retaining the original order.
puniq() {
    echo "$1" |tr : '\n' |nl |sort -u -k 2,2 |sort -n |
    cut -f 2- |tr '\n' : |sed -e 's/:$//' -e 's/^://'
}

#-------------------------------------------------------------------------------
# Bash completion helpers
#-------------------------------------------------------------------------------
source ~/.bash.d/git-completion.bash

#-------------------------------------------------------------------------------
# RVM
#-------------------------------------------------------------------------------
if [[ -s "$HOME/.rvm/scripts/rvm" ]]  ; then source "$HOME/.rvm/scripts/rvm" ; fi

#-------------------------------------------------------------------------------
# User Shell Environment
#-------------------------------------------------------------------------------
# Set java home
JAVA_HOME="/usr/lib/jvm/java-6-sun/"
export JAVA_HOME

# Condense path variables
PATH=$(puniq $PATH)
MANPATH=$(puniq $MANPATH)

# Set default prompt if interactive
test -n "$PS1" &&
prompt_color

