#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Sitelist
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📑

page=$(cat $(pwd)/sitelist | choose | sed -E "s/^(.*)#.*/\1/")
if [ -n "$page" ]; then
	open "$page"
fi
