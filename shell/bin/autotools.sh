#!/usr/bin/env bash
set -euo pipefail

project=${PROJECT:-$(basename $(pwd -P))}
version=${VERSION:-"0.1"}
bugmail=${BUGMAIL:-"christian.a.romney+bugs-$project@gmail.com"}

# generate Makefile.am

PKGLIBS="glib-2.0"
echo "bin_PROGRAMS=$project" > Makefile.am
echo "AM_CFLAGS=-g -O3" >> Makefile
echo "${project}_CFLAGS=$(pkg-config --cflags ${PKGLIBS})"' $(AM_CFLAGS)' >> Makefile.am
echo "${project}_LDADD=$(pkg-config --libs ${PKGLIBS})" >> Makefile.am

# run autoscan to generate configure.scan  template
autoscan

# replace the project-specific settings and create configure.ac
sed -e "s/FULL-PACKAGE-NAME/${project}/" \
    -e "s/VERSION/${version}/" \
    -e "s/BUG-REPORT-ADDRESS/${bugmail}/" \
    -e '10i\
AM_INIT_AUTOMAKE' < configure.scan > configure.ac

touch NEWS README AUTHORS ChangeLog
autoreconf -iv
./configure
make distcheck
