#!/bin/sh

set -e

mkdir ~/lms
cd ~/lms

git clone https://github.com/TiarkRompf/virtualization-lms-core
cd virtualization-lms-core
sbt publish-local
