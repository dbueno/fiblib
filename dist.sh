#!/bin/bash

FIBLIB_VERSION=`cat VERSION`

TAR_FILENAME=fiblib-${FIBLIB_VERSION}.tar.gz

echo Creating distribution "$TAR_FILENAME..."

tar czf $TAR_FILENAME . --exclude "${TAR_FILENAME}" \
  --exclude ".svn" --exclude "._bcdi" --exclude "._d" --exclude "._ncdi" \
  --exclude "dist.sh" --exclude ".depend"

