#!/bin/bash


# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add Bookmark
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📖
# @raycast.argument1 { "type": "text", "placeholder": "Description", "optional": true }

pbcopy < /dev/null
osascript copyselected.scpt
bookmark=$(pbpaste)

if [ -z $(echo "$bookmark" | grep "http") ]; then
	echo "No site selected"
	exit 0
fi

if [ -n "$1" ]; then
	bookmark="${1} | ${bookmark}"
fi
echo "$bookmark" >> bookmarks
echo "Bookmark added"
