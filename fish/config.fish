# fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

# Status Chars
set __fish_git_prompt_char_dirtystate '🚧'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

function fish_prompt
        set last_status $status
 	set_color green
	printf "%s" (whoami)
	set_color cyan
	printf "@%s" (hostname|cut -d . -f 1)
	set_color normal
        printf '%s ' (__fish_git_prompt)
       set_color normal
end

function fish_right_prompt
	set_color brgrey
	printf "%s" (pwd)
end

function fish_greeting
	 # no greeting
end
# Alias
alias j=jobs
alias lv="lv -c"
alias jq="jq -C"
alias ec="emacsclient -nw"
alias ecn="emacsclient"
alias rm="rm -f"

# ロケール
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8

# 単語の区切り文字
#export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>-_/'
#WORDCHARS=${WORDCHARS:s,/,,}
#WORDCHARS=${WORDCHARS:s,_,,}

# gd でディレクトリスタックから移動先を選択
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'

# Go
export GOROOT=/home/hiro/goroot/current
export GOPATH=/home/hiro/go
alias gp="cd $GOPATH/src/github.com/hironobu-s"

# Google Cloud SDK
bass source '/home/hiro/google-cloud-sdk/path.bash.inc'

# ConoHa environments
export OS_USERNAME=gncu47070904
export OS_PASSWORD="Hironobu123*"
export OS_AUTH_URL=https://identity.tyo1.conoha.io/v2.0
export OS_REGION_NAME=tyo1
export OS_TENANT_ID=6150e7c42bab40c59db53d415629841f

# PATH
set PATH $PATH ~/local/bin
set PATH $PATH $GOROOT/bin
set PATH $PATH $GOPATH/bin
