###################################################################################


# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
#ZSH_THEME="gitster"
ZSH_THEME="geoffgarside"
#ZSH_THEME="zhann"


# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# plugins=(git)

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


# ----------------------------------------------------------------------------------------------------------

# TERM
export TERM=xterm-256color

# PATH
#export PATH="~/local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
export PATH="~/.local/bin:/home/hiro/local/bin":$PATH

# Alias
alias j=jobs
alias ls="ls --color"
alias ll="ls -lhAF --color"
alias lv="lv -c"
alias jq="jq -C"

# ロケール
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8

## Emacsキーバインドを使う。
bindkey -e

# ヒストリ
HISTFILE=$HOME/.zsh-history
HISTSIZE=10000
SAVEHIST=10000

## ヒストリファイルにコマンドラインだけではなく実行時刻と実行時間も保存する。
setopt extended_history
## 同じコマンドラインを連続で実行した場合はヒストリに登録しない。
setopt hist_ignore_dups
## スペースで始まるコマンドラインはヒストリに追加しない。
setopt hist_ignore_space
# ヒストリを共有する
setopt share_history

# 単語の区切り文字
#export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>-_/'
WORDCHARS=${WORDCHARS:s,/,,}
WORDCHARS=${WORDCHARS:s,_,,}

# プロンプトを変更
# PROMPT='[%{$fg[cyan]%}%n%{$reset_color%}@%{$fg[magenta]%}%m%{$reset_color%}]%(!.#.$) '
PROMPT='%{$fg[cyan]%}%n@%m%{$reset_color%}:%{$fg[green]%}%c%{$reset_color%}$(git_prompt_info) %(!.#.$) '
RPROMPT='%{$fg[white]%}%~%{$reset_color%}'

## ディレクトリ名だけでcdする。
setopt auto_cd
## cdで移動してもpushdと同じようにディレクトリスタックに追加する。
setopt auto_pushd

# gd でディレクトリスタックから移動先を選択
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'

# https://github.com/zsh-users/zsh-completions
plugins+=(zsh-completions)
autoload -U compinit && compinit

## --prefix=~/localというように「=」の後でも
## 「~」や「=コマンド」などのファイル名展開を行う。
setopt magic_equal_subst

## jobsでプロセスIDも出力する。
setopt long_list_jobs

## 実行したプロセスの消費時間が3秒以上かかったら
## 自動的に消費時間の統計情報を表示する。
REPORTTIME=3

## ページャーを使いやすくする。
### grep -r def *.rb L -> grep -r def *.rb |& lv
alias -g L="| $PAGER"
## grepを使いやすくする。
alias -g G='| grep'

# GVM
[[ -s "/home/hiro/.gvm/scripts/gvm" ]] && source "/home/hiro/.gvm/scripts/gvm"

# rbenv
#[[ -d ~/.rbenv  ]] && \
#  export PATH=${HOME}/.rbenv/bin:${PATH} && \
#  eval "$(rbenv init -)"

# Aliases
alias ec="emacsclient -n"

source /home/hiro/.gvm/scripts/gvm
