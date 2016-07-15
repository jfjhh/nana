#!/bin/sh -x

DIRS="bin bios data exe include out src"

for DIR in $DIRS; do
	mkdir -p $DIR .deps/$DIR
done

make

