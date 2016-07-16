#!/bin/sh

replace_var()
{
	sed -i "s/@${1}@/${2}/g" "${3}"
}

CONFIG="config.mk"

DEPENDENCIES="
nasm
gcc
make
sed
ed
dd
fdisk
mformat
mcopy
mdir
touch
cp
rm
printf
"

printf "\033[0;1m*** Checking Dependencies ***\033[0m\n\n"
for DEP in ${DEPENDENCIES}; do
	printf "%12s ... " ${DEP}
	D="$(which ${DEP})"
	if [ -z "${D}" ]; then
		printf "\n"
		printf "%12s ... " "sudo ${DEP}"
		D="$(sudo which ${DEP})"
		if [ -z "${D}" ]; then
			printf "\033[0;31mNot Found!\033[0m\n"
			exit 1
		fi
	fi
	printf "\033[0;32m%s\033[0m\n" ${D}
done

printf "\n\033[0;1m*** All Dependencies OK! ***\033[0m\n"

set -xv

[ -f "${CONFIG}.in" ] \
	&& cp ${CONFIG}.in ${CONFIG}

VERSION="$(git tag | head)"
[ -z ${VERSION} ] \
	&& VERSION="v0.0.1"
replace_var "VERSION" ${VERSION} ${CONFIG}

BRANCH="$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')"
PROJNAME="$(basename `pwd`)"
[ -n "${BRANCH}" -a "${BRANCH}" != "master" ] \
	&& PROJNAME="${PROJNAME}-${BRANCH}"
replace_var "PROJNAME" ${PROJNAME} ${CONFIG}

for ARG in $*; do
	printf "%s" ${ARG} >> ${CONFIG}
done

printf "\n\033[0;1m*** You may now run \`make\`. ***\033[0m"

