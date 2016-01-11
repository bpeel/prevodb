#!/bin/sh

test -n "$srcdir" || srcdir=`dirname "$0"`
test -n "$srcdir" || srcdir=.

olddir=`pwd`

cd "$srcdir"

test -f src/main.c || {
	echo "You must run this script in the top-level source directory"
	exit 1
}

# GNU gettext automake support doesn't get along with git.
# https://bugzilla.gnome.org/show_bug.cgi?id=661128
touch -t 200001010000 po/prevo.pot

AUTORECONF=`which autoreconf`
if test -z $AUTORECONF; then
        echo "*** No autoreconf found, please install it ***"
        exit 1
fi

# NOCONFIGURE is used by gnome-common
if test -z "$NOCONFIGURE"; then
        if test -z "$*"; then
                echo "I am going to run ./configure with no arguments -" \
                    "if you wish "
                echo "to pass any to it, please specify them on the $0" \
                    "command line."
        fi
fi

rm -rf autom4te.cache

autoreconf --force --install --verbose || exit $?

cd "$olddir"
test -n "$NOCONFIGURE" || "$srcdir/configure" "$@"
