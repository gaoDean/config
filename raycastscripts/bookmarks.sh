#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Bookmarks
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📖

killall -q choose
page=$(tac $(pwd)/bookmarks |
	choose -c "F4DBD6" -w 45 |
	sed -E "s/.* | (.*)/\1/")

if [ -n "$page" ]; then
	open "$page"
fi
