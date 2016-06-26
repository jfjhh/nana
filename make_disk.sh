#!/bin/sh

if [ -n "$1" -a ! -f "$1" ]; then
	dd if=/dev/zero of="$1" bs=512 count=16128
	sudo fdisk "$1" <<-FDISK
	p
	o
	n
	p
	1
	2048

	a
	t
	b
	p
	w
	FDISK
else
	echo "Need a disk filename!" >&2
fi

