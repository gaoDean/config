#!/bin/bash


# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add Password
# @raycast.mode fullOutput
#
# Optional parameters:
# @raycast.icon 🗝️
# @raycast.argument1 { "type": "text", "placeholder": "Account", "optional": false }

if [ $# -eq "0" ]; then
	echo "No arguments"
	exit 0
fi
~/repos/bin/pa/pa add "$1"
