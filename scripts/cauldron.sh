#!/bin/sh

platform='unknown'
unamestr=`uname`

if [ "$unamestr" = 'OpenBSD' ]; then platform='openbsd'
elif [ "$unamestr" = 'Linux' ]; then platform='linux'
fi

if [ "$platform" = 'openbsd' ]; then
  chicken-csi -s alchemy.scm
else
  csi -s alchemy.scm
fi
