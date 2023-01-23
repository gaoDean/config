#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Bookmarks
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🔎

page=$(cat $(pwd)/bookmarks | choose | sed -E "s/^(.*)#.*/\1/")
if [ -n "$page" ]; then
	open "$page"
fi
