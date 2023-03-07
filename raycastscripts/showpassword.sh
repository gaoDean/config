#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Passwords
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🔑

killall -q choose
password=$(~/repos/bin/pa/pa list | choose)
if [ -n "$password" ]; then
	username=$(echo "$password" | cut -d: -f 2)
	if [ -n "$username" ]; then
		osascript type.scpt "$username"
	fi
	~/repos/bin/pa/pa show "$password" | pbcopy
	(~/.local/bin/pbclear 30 &) &> /dev/null
fi
