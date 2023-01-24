#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Bookmarks
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📖

page=$(cat $(pwd)/bookmarks |
	choose -c "F4DBD6" -w 45 |
	perl -pe "s|^(.*?)\s*#.*|\1|")
if [ -n "$page" ]; then
	open "$page"
fi
