#!/bin/sh

set -e

sbt compile
sbt test
