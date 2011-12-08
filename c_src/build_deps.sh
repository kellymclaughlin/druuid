#!/bin/bash

set -e

UUID_VSN=1.6.2

if [ `basename $PWD` != "c_src" ]; then
    pushd c_src
fi

BASEDIR="$PWD"

case "$1" in
    clean)
        test -f uuid-$UUID_VSN/.libs/libuuid.a || exit 0

        (cd uuid-$UUID_VSN && \
            make distclean)
        ;;

    *)
        test -f uuid-$UUID_VSN/.libs/libuuid.a && exit 0

        (cd uuid-$UUID_VSN && \
            ./configure --disable-shared --enable-static --with-pic \
            && make)

        ;;
esac

