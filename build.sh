#!/usr/bin/env sh
set -e
cd "$(dirname "$0")"
g++ -Icore -Wall -O2 -DNDEBUG cli/Unity.cpp -o chc 
#g++ -Icore -Wall -DNDEBUG cli/Unity.cpp -o chc 
