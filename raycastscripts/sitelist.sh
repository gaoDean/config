#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Sitelist
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📑

page=$(tac $(pwd)/sitelist |
	choose -c "F4DBD6" -w 55 |
	sed -E "s/.* | (.*)/\1/")
if [ -n "$page" ]; then
	open "$page"
fi
