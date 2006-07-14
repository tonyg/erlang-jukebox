#!/bin/sh
"$@" &
echo $!
wait $! > /dev/null 2>&1
