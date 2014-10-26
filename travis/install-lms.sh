#!/bin/sh

set -e

if ! [ -d "dependencies" ]; then
    mkdir "dependencies"
fi

cd "dependencies"
# get a forked version that lifts more standard library operations
# e.g. on Map and Seq
git clone "https://github.com/markus1189/virtualization-lms-core"

cd "virtualization-lms-core"

sbt compile
