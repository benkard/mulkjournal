#! /bin/sh
DIR=`dirname "$0"`
exec clisp -M "$DIR/lispinit.mem.gz" "$DIR/journal.lisp" "$@"
