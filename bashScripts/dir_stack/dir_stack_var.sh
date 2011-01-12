
# push te directory onto the top of the stack; 
# Only first parameter is considered.
# More parameters if present will be ignored.
function dpush()
{
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

	# Stack is empty
	if [ -z ${DIRECTORY_STACK} ]
	then
		DIRECTORY_STACK[0]=$1
	else	# New TOS is length(stack) i.e. max_index+1
		DIRECTORY_STACK[${#DIRECTORY_STACK[*]}]=$1
	fi

	echo ${DIRECTORY_STACK[*]}
	cd $1

	return 0
}

function dpop()
{
	if [ $# != 3 -o $# -o 1 ] 
	then
		echo "dpop: Usage: 
}

function didx()
{
	echo "blah";
}

function dclr()
{
	echo "blah";
}
