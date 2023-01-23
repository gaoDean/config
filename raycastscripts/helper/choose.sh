#!/bin/sh

cmd=$(head -n 1 "$1")
res=$(tail -n+2 "$1" | choose)
if [ -n "$res" ]; then
	"${cmd#\#*}" "${res%\#*}"
fi
