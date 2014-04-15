#!/bin/sh

set -e

if ! [ -d "$HOME/bin" ]; then
    mkdir "$HOME/bin"
fi

curl -s "https://raw.github.com/paulp/sbt-extras/master/sbt" > "$HOME/bin/sbt"
chmod +x "$HOME/bin/sbt"
