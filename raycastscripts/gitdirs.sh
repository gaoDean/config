#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Update all gitdirs
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📄

while read file
do
	dir=$(dirname file)
	git -C "$dir" add -A
	git -C "$dir" commit -m 'update' -q
	git -C "$dir" push -q
done < ~/.config/shell/gitdirs
