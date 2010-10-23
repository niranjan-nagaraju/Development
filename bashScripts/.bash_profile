function cdf() {
	mkdir -p $1 && cd $1
}

function cx() {
	chmod +x "$@"
}

function c-x() {
	chmod -x "$@"
}

function build_tags() {
	~/Development/build_tags.sh
}

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

function c_env() {
	export DYLD_LIBRARY_PATH=~/Development/C/common/out:~/Development/C/Algorithms/sorts/out
}

export PS1='[\t \u \W]$ '
alias ls='ls -G'
export PATH=$PATH:/usr/texbin:/opt/git/bin
export EDITOR=vim
build_tags
c_env

