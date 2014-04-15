#!/bin/sh

set -e

mkdir ~/bin
cd ~/bin

curl -s https://raw.github.com/paulp/sbt-extras/master/sbt > sbt
chmod +x sbt

cd $HOME
