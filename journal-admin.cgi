#! /bin/sh
DIR=`dirname "$0"`
if test -e /home/mulk; then
    # MST-plus.
    LISPINIT_DIR="$DIR"
else
    # NearlyFreeSpeech.NET.
    LISPINIT_DIR=/home/protected/journal
fi

exec clisp -M "$LISPINIT_DIR/lispinit.mem.gz" "$DIR/run.lisp" --admin-mode
