#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Passwords Only
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🔑

password=$(~/repos/bin/pa/pa list | choose)
if [ -n "$password" ]; then
	~/repos/bin/pa/pa show "$password" | pbcopy
	(~/.local/bin/pbclear 30 &) &> /dev/null
fi
