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
export PS1='[\t \W]$ '
alias ls='ls -G'

# Git and pyqt4 paths
export PATH=$PATH:/usr/texbin:/opt/git/bin:~/Development/PyQt4/Qt4/bin:/System/Library/Frameworks/Python.framework/Versions/2.6/bin/

# python libraries/modules path
export PYTHONPATH=$PYTHONPATH:/Users/Niranjan/Development/python/:/Library/Python/2.6/site-packages/

# MIT-Scheme Core
export MITSCHEME_LIBRARY_PATH="/Applications/MIT\:GNU\ Scheme.app/Contents/Resources"

# build the ctags
build_tags

# Make C environment DYLD_LIBRARY_PATH available
c_env


