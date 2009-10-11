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

lisp_mtime=0
for x in $DIR/*.lisp; do
    mtime=`mtime_of "$x"`
    if [ $mtime -gt $lisp_mtime ]; then
        lisp_mtime=$mtime
    fi
done

if ! [ -f "$FASL_FILE" -a \( `mtime_of "$FASL_FILE"` -gt $lisp_mtime \) ]; then
    env LC_ALL=de_DE.UTF-8 clisp -M "$LISPINIT_DIR/lispinit.mem.gz" "$DIR/compile.lisp" &&\
    find "$DIR" -name "*.fas" -print0 | xargs -0 cat > "$FASL_FILE"
fi

exec env LC_ALL=de_DE.UTF-8 clisp -q -q -M "$LISPINIT_DIR/lispinit.mem.gz" -x "(progn (load \"$FASL_FILE\") (cl-user::script-main))" --admin-mode
