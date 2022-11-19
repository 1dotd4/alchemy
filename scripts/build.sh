#!/bin/sh

platform=`uname`
build=csc

mkdir -p dist

cp alchemy.scm dist/cauldron-bin.scm


if [ "$platform" = 'OpenBSD' ]; then build=chicken-csc ; fi

$build dist/cauldron-bin.scm
