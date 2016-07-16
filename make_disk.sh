#!/bin/sh -xv

usage()
{
	echo <<-USAGE
	Usage: \`${SELF} DATA DISK [[[sector offset] block size] count]\`." >&2
	USAGE
	[ -n "$1" ] && exit $1 || exit 255
}

BS=512
COUNT=16128
START_SEC=2048

DISK="$1"
[ -n "$2" ] && START_SEC="$2"
[ -n "$3" -a -n "$4" ] && BS="$3" && COUNT="$4"
BYTES=$(( ${BS} * ${COUNT} ))

SELF="$(basename -s.sh $0)"
SHELL_NAME="$(basename -s.sh $_)"
[ -z "${SELF}" -o "${SELF}" = "${SHELL_NAME}" ] \
	&& SELF="make_disk" \
	&& SRC=true \
	|| SRC=false

if [ ${START_SEC} -gt 0 ] && \
	[ ${START_SEC} -le 4096 ] && \
	[ ${START_SEC} -lt ${BYTES} ] && \
	[ -n "${DISK}" ]
then
	dd if=/dev/zero of="${DISK}" bs="${BS}" count="${COUNT}"
	sudo fdisk "${DISK}" <<-FDISK
	p
	o
	n
	p
	1
	${START_SEC}

	a
	t
	b
	p
	w
	FDISK
else
	usage 2
fi

