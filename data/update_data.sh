#!/bin/sh

LIST="$1"
WRITE_DATA="$2"
DISK="$3"

[ -f "$LIST" -a -x "$WRITE_DATA" -a -w "$DISK"  ] && \
	while read LINE; do
		FILE="$(echo $LINE | cut -f 1 -d,)"
		SECTOR="$(echo $LINE | cut -f 2 -d,)"
		[ "$FILE" -nt "$DISK" -o -f ".newdisk" ] && [ -n "$SECTOR" ] \
			&& echo "Updating '$FILE'." \
			&& ./$WRITE_DATA $FILE $DISK $SECTOR
	done < "$LIST" >&2

	[ -f ".newdisk" ] && rm ".newdisk" || true

