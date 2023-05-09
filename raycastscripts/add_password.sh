#!/bin/bash


# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add Password
# @raycast.mode fullOutput
#
# Optional parameters:
# @raycast.icon 🗝️
# @raycast.argument1 { "type": "text", "placeholder": "Site", "optional": false }
# @raycast.argument2 { "type": "text", "placeholder": "Account", "optional": true }
# @raycast.argument2 { "type": "text", "placeholder": "Password", "optional": false }

if [ $# -eq "0" ]; then
	echo "No arguments"
	exit 0
fi
emacs --batch --eval "(pa--add $1 $2 $3)"
