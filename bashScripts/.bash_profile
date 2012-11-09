# Create a directory if it doesn't exist and change to it
function cdf() {
	mkdir -p $1 && cd $1
}

# Move up <x> levels, or just one level if no level specified
function ..(){
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

