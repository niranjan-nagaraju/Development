# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="~/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="agnoster"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

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
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

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

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd beep extendedglob nomatch notify
bindkey -e
# End of lines configured by zsh-newuser-install

## added by Anaconda2 4.4.0 installer
export PATH=$PATH:~/anaconda/bin

function cdf() {
	mkdir -p $1 && cd $1
}

# Move up <x> levels, or just one level if no level specified
function ..() {
	if [ -z $1 ]
	then
		cd ..
	else
		_dir_path=
		for i in `seq $1`
		do
			_dir_path=${_dir_path}../
		done
		echo "cd ${_dir_path}"
		cd ${_dir_path}
	fi
}

# chmod +x
function cx() {
	chmod +x "$@"
}

# chmod -x
function c-x() {
	chmod -x "$@"
}


# Build ctags at startup
function build_tags() {
	~/Development/build_tags.sh
}

# switch to development directory - c/c++/python/haskell
function dev() {
	case $1 in
		"c+" | "c++" | "cpp" )	set c++ ;;
		"py" | "python" )		set python ;;
		"hs" | "haskell" )		set haskell ;;
	esac

	if test -d ~/Development/$1
	then
		cd ~/Development/$1
	fi
}
# Set Mac c_env - specifically Mac's version of LD_LIBRARY_PATH
function c_env() {
	export DYLD_LIBRARY_PATH=~/Development/C/common/out/mac:~/Development/C/test_common/out/mac:~/Development/C/Algorithms/sorts/out/mac
}

# Shell command editor - c-x c-e should open vim.
# Why? Because '.'
# Thats why
export EDITOR=vim

# run emacs in console mode
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'

# Display prompt: [hh:mm:ss <directory>]
#export PS1='[\t \W]$ '
alias ls='ls -G'

# Git and pyqt4 paths
export PATH=$PATH:/usr/texbin:/opt/git/bin:~/Development/PyQt4/Qt4/bin:/System/Library/Frameworks/Python.framework/Versions/2.6/bin/

# adb path
export PATH=$PATH:~/Library/Android/sdk/platform-tools

# python libraries/modules path
export PYTHONPATH=$PYTHONPATH:/Volumes/Storage/Projects/Github/Development/python:/Library/Python/2.6/site-packages/

# MIT-Scheme Core
export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"

# build the ctags
#build_tags

# Make C environment DYLD_LIBRARY_PATH available
c_env


# auto-complete in python2 interpreter shells
export PYTHONSTARTUP=~/.pythonrc 


export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_191.jdk/Contents/Home
export PATH=$PATH:/usr/local/opt/mongodb-community@4.0/bin

# go to gitlab repos
alias gitlab='cd /Volumes/Storage/Machi/gitlab'

# go to github repos
alias github='cd /Volumes/Storage/Projects/Github/'

# use nvim
alias v=nvim


export PATH="$HOME/.cargo/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
