#!/bin/bash
# Script to open file in an existing frame if one exists, or create a new one otherwise.
# Thanks, Stack Overflow!
# http://superuser.com/a/862809

emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t
if [ "$?" = "1" ]; then
    emacsclient -c -n -a "" "$@"
else
    emacsclient -n -a "" "$@"
fi
