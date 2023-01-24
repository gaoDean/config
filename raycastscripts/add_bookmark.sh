#!/bin/bash


# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add Bookmark
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📖
# @raycast.argument1 { "type": "text", "placeholder": "Description", "optional": true }

osascript copyselected.scpt
bookmark=$(pbpaste)
if [ -n "$1" ]; then
	bookmark="${bookmark} # ${1}"
fi
echo "$bookmark" >> bookmarks
echo "Bookmark added"
