#!/bin/sh -xv

DEPDIRS="data src include"

for DIR in $DEPDIRS; do
	mkdir -p .deps/$DIR
done

