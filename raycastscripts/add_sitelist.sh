#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add Sitelist
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📑
# @raycast.argument1 { "type": "text", "placeholder": "Description", "optional": true }

pbcopy < /dev/null
osascript copyselected.scpt
site=$(pbpaste)

if [ -z $(echo "$site" | grep "http") ]; then
	echo "No site selected"
	exit 0
fi

if [ -n "$1" ]; then
	site="${1} | ${site}"
fi
echo "$site" >> sitelist
echo "Site added"
