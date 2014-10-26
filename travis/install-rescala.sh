#!/bin/sh

set -e

if ! [ -d "dependencies" ]; then
    mkdir "dependencies"
fi

cd "dependencies"

git clone "https://github.com/guidosalva/REScala.git"
cd "REScala"

# We need a version that works with Scala 2.10
git reset --hard abb2d0cf5de58f404baac2f1b72b547635444d58

sbt compile
