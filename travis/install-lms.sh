#!/bin/sh

set -e

if ! [ -d "dependencies" ]; then
    mkdir "dependencies"
fi

cd "dependencies"

git clone "https://github.com/TiarkRompf/virtualization-lms-core"

cd "virtualization-lms-core"

sbt compile
