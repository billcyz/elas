#!/bin/bash
# ELAS shell command line interface

DEFAULT_PORT=8080
#LOG_FILE=$HOME

# Help Function
help() {
	echo "===============ELAS=================="
	echo "=      COMMAND HELP INFORMATION     ="
	echo "====================================="
	echo ""
	echo "Usage: ./`basename $0` [OPTIONS]"
	echo "-----------------------------------------------"
	echo "	[OPTIONS]		   [DESCRIPTION]"
	echo "	start			Start ELAS service."
	echo "	stop			Stop ELAS service."
	echo "	test			Test ELAS service."
	echo "	add-project		Add project."
	echo "	delete-project		Delete project."
	echo "	add-resources		Add project resources."
	echo "	delete-resources	Delete project resources."
}

# Start ELAS
start() {
	SERVICE_PORT=$1
	`which erl` -sname elas_$SERVICE_PORT
}
#start $DEFAULT_PORT

# Start function based on arguments
case "$1" in
	-h | --help)
		help
	;;
	start)
		echo start
	;;
esac