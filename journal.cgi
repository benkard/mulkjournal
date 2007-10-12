#! /bin/sh
DIR=`dirname "$0"`
if test -e /home/mulk; then
    # MST-plus.
    LISPINIT_DIR="$DIR"
else
    # NearlyFreeSpeech.NET.
    LISPINIT_DIR=/home/protected/journal
fi

exec env LC_ALL=de_DE.UTF-8 clisp -M "$LISPINIT_DIR/lispinit.mem.gz" "$DIR/run.lisp"
