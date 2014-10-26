#!/bin/sh

set -e

if ! [ -d "dependencies" ]; then
    mkdir "dependencies"
fi

cd "dependencies"

git clone "https://github.com/guidosalva/REScala.git"
# We need a version that works with Scala 2.10
git reset --hard abb2d0cf5de58f404baac2f1b72b547635444d58
cd "REScala"

sbt compile
