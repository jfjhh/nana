#!/bin/sh -xv

NEWDISK="data/.newdisk"
LIST="$1"
shift
WRITE_DATA="$1"
shift
DISK="$1"

[ -f "$LIST" -a -x "$WRITE_DATA" -a -w "$DISK"  ] && \
	while read LINE; do
		FILE="$(echo $LINE | cut -f 1 -d,)"
		SECTOR="$(echo $LINE | cut -f 2 -d,)"
		[ "$FILE" -nt "$DISK" -o -f "$NEWDISK" ] && [ -n "$SECTOR" ] \
			&& echo "Updating '$FILE'." \
			&& $WRITE_DATA $FILE $DISK $SECTOR
	done < "$LIST" >&2

	[ -f "$NEWDISK" ] && rm "$NEWDISK" || true

