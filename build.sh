#!/bin/bash
set -e

# Create target directories.
mkdir -p web/
mkdir -p bin/

# Build the Go binary.

cd server; 
go get -v -t -d ./...
if [ $# -eq 0 ]
  then
    echo "Building for debug..."
    go build -o ../bin/server;
  else 
    echo "Static linking for docker..."
    CGO_ENABLED=0 GOOS=linux go build -o ../bin/server -a -installsuffix cgo .
fi
cd ..

# Build the elm outputs.
cd js; elm-make Main.elm --yes --output=../web/elm.js; cd ..

