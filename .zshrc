if [ -e $HOME/.profile ]; then
    . $HOME/.profile
fi

# alias
alias su_zsh='sudo -H -s'
case "${OSTYPE}" in
freebsd*|darwin*)
    alias ls="ls -G -w"
    ;;
linux*)
    alias ls="ls --color"
    alias open="gnome-open"
    ;;
esac

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'

# colors
autoload colors
colors

setopt auto_cd
setopt auto_pushd
setopt list_packed
setopt noautoremoveslash
setopt nolistbeep

# history
autoload history-search-end
HISTFILE=${HOME}/.zsh_history
HISTSIZE=50000
SAVEHIST=50000
setopt hist_ignore_dups
setopt share_history


# key bind
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end

bindkey "\e[Z" reverse-menu-complete

# completion
fpath=(${HOME}/.zsh/functions/Completion ${fpath})
autoload -U compinit
compinit


# prompt
case ${UID} in
    xterm*|kterm*|rxvt*)
        PROMPT=$(print "%B%{\e[34m%}%m:%(5~,%-2~/.../%2~,%~)%{\e[33m%}%# %b")
        PROMPT=$(print "%{\e]2;%n@%m: %~\7%}$PROMPT") # title bar
        ;;
    *)
        PROMPT='%m:%c%# '
        ;;
esac

# added by travis gem
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh


# git utility with peco
alias -g   peco_branch='git branch -a | peco'
alias -g            gb='git branch -a | peco'
alias -g    peco_stash='git stash list | peco'
alias -g   peco_commit='echo_first `git log --branches --no-merges --oneline | peco`'
alias -g   peco_remote='git remote | peco'

alias -g    git_rm_branch='git branch -D `peco_branch`'
alias -g               gr='git branch -D `peco_branch`'
alias -g     git_checkout='git checkout `peco_branch`'
alias -g               gc='git checkout `peco_branch`'
alias -g    git_stash_pop='git stash pop `peco_stash`'
alias -g              gsp='git stash pop `peco_stash`'
alias -g   git_stash_drop='git stash drop `peco_stash`'
alias -g              gsd='git stash drop `peco_stash`'
alias -g          git_log='git log --graph --decorate --oneline'
alias -g               gl='git log --graph --decorate --oneline'
alias -g  git_cherry-pick='git cherry-pick `peco_commit`'
alias -g              gcp='git cherry-pick `peco_commit`'

function echo_first {
  echo $1
}

# z
. `brew --prefix`/etc/profile.d/z.sh

# change directory with z
function peco-z-search() {
  which peco z > /dev/null
  if [ $? -ne 0 ]; then
    echo "Please install peco and z"
    return 1
  fi
  local res=$(z | sort -rn | cut -c 12- | peco)
  if [ -n "$res" ]; then
    BUFFER+="cd $res"
    zle accept-line
  else
    return 1
  fi
}
zle -N peco-z-search
bindkey '^[' peco-z-search


# change directory with ghq list
function peco-src () {
    which peco ghq > /dev/null
    if [ $? -ne 0 ]; then
      echo "Please install peco and ghq"
      return 1
    fi
    local selected_dir=$(ghq list --full-path | peco --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-src
bindkey '^]' peco-src

powerline_config=~/src/github.com/powerline/powerline/powerline/bindings/zsh/powerline.zsh
if [ -f "$powerline_config" ]
then
    powerline-daemon -q
    . ~/src/github.com/powerline/powerline/powerline/bindings/zsh/powerline.zsh
fi
if which swiftenv > /dev/null; then eval "$(swiftenv init -)"; fi
