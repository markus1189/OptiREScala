#!/bin/sh

set -e

if ! [ -d "dependencies" ]; then
    mkdir "dependencies"
fi

cd "dependencies"

git clone "https://github.com/guidosalva/REScala.git"

cd "REScala"

sbt compile
