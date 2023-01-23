#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add Sitelist
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📑
# @raycast.argument1 { "type": "text", "placeholder": "Description", "optional": true }

osascript copyselected.scpt
site=$(pbpaste)
if [ -n "$1" ]; then
	site="${site} # ${1}"
fi
echo "$site" >> sitelist
echo "Site added"
