#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Passwords
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🔎

password=$(~/repos/bin/pa/pa list | choose)
if [ -n "$password" ]; then
	echo $(echo "$password" | cut -d: -f 2) $(~/repos/bin/pa/pa show "$full")
	~/repos/bin/pa/pa show "$password" | pbcopy
	(pbclear 30 &) &> /dev/null
fi
