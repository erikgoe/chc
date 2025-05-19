#!/usr/bin/env sh
set -e
cd "$(dirname "$0")"
g++ -Icore -Wall -O2 cli/Unity.cpp -o chc 
