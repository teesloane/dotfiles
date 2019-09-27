#!/usr/bin/env bash

# requires: AnyBar (brew cask install anybar)
# this is a script that periodically pings emacs to see if org-clock is running
# and then displays a green dot if clock is running or red it not.
#
# measure process with: top -pid $(pgrep -f org-clock-check.sh)

function anybar {
	echo -n $1 | nc -4u -w0 localhost ${2:-1738};
}

function checkAnybar() {
	if pgrep AnyBar; then
		echo 'Running';
	else
		open -a AnyBar
	fi
}

function runLight() {
	
	checkAnybar

	while [ true ]; do
		if [[ $(emacsclient -e '(org-clocking-p)') = "t" ]]; then
			anybar green
			# echo -n "green" | nc -4u -w0 localhost 1738;
		else
			anybar red
			# echo -n "red" | nc -4u -w0 localhost 1738
		fi
		sleep 1
	done
}

function killProcess() {
	local SCRIPT_ID = pgrep -f org-clock-check.sh
	local ANYBAR_ID = pgrep -f AnyBar
	anybar red
	sleep 1

	echo "Ending process id: script: $(SCRIPT_ID) and anybar: $(ANYBAR_ID)"
	kill SCRIPT_ID
}


if [ "$1" == "run" ]
then
	runLight
elif [ "$1" == "stop" ]
then
	killProcess
else
	echo "Gimme a command: 'run' or 'stop'"
fi
