#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add Bookmark
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📖

osascript copyselected.scpt
pbpaste >> bookmarks
echo "Bookmark added"
