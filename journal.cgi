#! /bin/sh
DIR=`dirname "$0"`
if test -e /home/mulk; then
    # MST-plus.
    LISPINIT_DIR="$DIR"
else
    # NearlyFreeSpeech.NET.
    LISPINIT_DIR="$NFSN_SITE_ROOT/protected/journal"
fi

mtime_of() {
    stat -n -f "%m" -t "%s" "$1" || echo -n 0
}

FASL_FILE="$LISPINIT_DIR/journal-full.fas"

most_recently_changed_lisp_file=`ls -rt $DIR/*.lisp | tail -n1`
lisp_mtime=`mtime_of $most_recently_changed_lisp_file`

if ! [ -f "$FASL_FILE" -a \( `mtime_of "$FASL_FILE"` -gt $lisp_mtime \) ]; then
    env LC_ALL=de_DE.UTF-8 clisp -M "$LISPINIT_DIR/lispinit.mem.gz" "$DIR/compile.lisp" &&\
    find "$DIR" -name "*.fas" -print0 | xargs -0 cat > "$FASL_FILE"
fi

exec env LC_ALL=de_DE.UTF-8 clisp -q -q -M "$LISPINIT_DIR/lispinit.mem.gz" -x "(progn (load \"$FASL_FILE\") (cl-user::script-main))"
