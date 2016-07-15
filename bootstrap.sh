#!/bin/sh -x

OUTDIRS="bin bios exe out obj"
DEPDIRS="data src include"

for DIR in $OUTDIRS; do
	mkdir -p $DIR
done

for DIR in $DEPDIRS; do
	mkdir -p .deps/$DIR
done

