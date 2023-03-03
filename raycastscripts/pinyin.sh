#!/bin/bash

# @raycast.title Pinyin

# @raycast.icon 🔠
# @raycast.mode silent
# @raycast.schemaVersion 1
# @raycast.argument1 { "type": "text", "placeholder": "", "optional": true }

pbcopy < /dev/null
osascript copyselected.scpt
selected=$(pbpaste)

if [ -z "$selected" ]; then
	exit 0
fi

echo "$selected" | /Users/deangao/.local/share/cargo/bin/pinyin-tool
