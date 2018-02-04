#!/bin/bash
set -e

# Create target directories.
mkdir -p web/
mkdir -p bin/

# Build the Go binary.
cd server; go build -o ../bin/server
