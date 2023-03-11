#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Update All Gitdirs
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📄

while read dir
do
	~/.local/bin/gaa "$dir"
done < ~/.config/shell/gitdirs
echo "All gitdirs updated"
