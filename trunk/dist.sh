#!/bin/bash

FIBLIB_VERSION=`cat VERSION`

echo Creating distribution "$FIBLIB_VERSION..."

tar cvzf fiblib-${FIBLIB_VERSION}.tar.gz .

