#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Update All Gitdirs
# @raycast.mode fullOutput
#
# Optional parameters:
# @raycast.icon 📄

while read dir
do
	git -C "$(eval $dir)" add -A
	git -C "$(eval $dir)" commit -m 'update'
	git -C "$(eval $dir)" push -q
done < ~/.config/shell/gitdirs
echo "All gitdirs updated"
