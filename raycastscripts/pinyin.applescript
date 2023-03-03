#!/usr/bin/osascript


do shell script "pbcopy < /dev/null; osascript copyselected.scpt; selected=$(pbpaste); echo "$selected" | /Users/deangao/.local/share/cargo/bin/pinyin-tool" with administrator privileges
