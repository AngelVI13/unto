#!/usr/bin/bash
VARIABLE="${2:-1}"
dune exec -- ./bin/main.exe update -d app.db -t new_tokens.json -n $1 -s $VARIABLE
