#!/bin/sh

set -e

echo "Building in '${pwd}'"

ls -A

sbt compile
sbt test

echo "Done"
