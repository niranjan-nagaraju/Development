# push the directory onto the top of the stack; 
# Only first parameter is considered.
# More parameters if present will be ignored.
function dpush()
{
	# On no arguments, behave like cd -; Swap {tos} and {tos-1}
	if [ $# == 0 ]
	then
		# There should be atleast 2 entries, One last working directory and one current directory
		if [ ${#DIR_STACK[*]} -lt 2 ]
		then
			echo "dpush: Stack empty"
			return 1;
		fi

		DIR_STACK=( ${DIR_STACK[1]} ${DIR_STACK[0]} ${DIR_STACK[*]:2} ) 

		cd $DIR_STACK

		return 0
	fi

	# Directory does not exist
	if [ ! -e $1 ]
	then
		echo "dpush: ${1}: Directory does not exist";
		return 1;
	fi

	# Parameter is not a directory
	if [ ! -d $1 ]
	then
		echo "dpush: ${1}: Not a directory"
		return 1;
	fi

	# Push new diretctory entry onto the top of the stack.
	DIR_STACK=( $1 ${DIR_STACK[*]} )

	echo ${DIR_STACK[*]}
	cd $1

	return 0
}

# Pop a directory entry of the top of the stack
# or remove an entry or one at the specified index
function dpop()
{
	# 1, 2 or 0 parameters are valid
	# No pop operation can be done on an empty stack
	if [ ${#DIR_STACK[*]} == 0 ]
	then
		echo "dpop: Stack empty"
		return 1
	fi

	case $# in
		0) # Remove directory entry off the top of the stack and cd to it
			stk_idx=`expr ${#DIR_STACK[*]} - 2`
			dir_entry=${DIR_STACK[$stk_idx]}
			unset DIR_STACK[$stk_idx]

			cd $dir_entry
			return 0
		;;

		1) # Remove specified directory if found
			dir_entry=`readlink -f $1`
			i=0
			for dir_in_stk in ${DIR_STACK[*]}
			do
				if [ $dir_in_stk == $dir_entry ]
				then
					unset DIR_STACK[$stk_idx]
					cd $dir_entry

					return 0
				fi
				(( i = $i + 1 ))
			done
			return 1
		;;

		2) # 2 arguments, an index is expected
			if [ $1 != '-i' ]
			then
				echo "dpop: Incorrect switch, -i expected"
				return 1
			fi
	
			shift;
			stk_idx=$1

			# Invalid index
			if [ $stk_idx < 0 -o $stk_idx >= ${#DIR_STACK[*]} ]
			then
				echo "dpop: Invalid Index"
				return 1
			fi
	
			i=0
			for dir_in_stk in ${DIR_STACK[*]}
			do
				if [ $stk_idx == $i ]
				then
					break
				fi
				(( i = $i + 1 ))
			done

			cd $dir_in_stk
			unset ${DIR_STACK[$i]}
			return 0
		;;

		*) # Invalid number of arguments
			echo "dpop: Usage: dpop [-i index] [directory]" 
			return 1
		;;

	esac
}

function didx()
{
	if [ ${#DIR_STACK[*]} == 0 ]
	then
		echo "didx: Stack empty"
		return 1
	fi

	for i in ${DIR_STACK[*]}
	do
		echo $i
	done
}

function dclr()
{
	unset DIR_STACK
}
