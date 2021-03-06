#! /bin/sh
clisp -q -q -on-error exit <<EOF
(dolist (system '(:cl-ppcre :cl-fad :iterate :cl-markdown :parenscript
                  :yaclml :lisp-cgi-utils :alexandria :xml-emitter
                  :split-sequence :clsql :clsql-sqlite3 :drakma :cybertyggr-time
                  :s-xml-rpc))
  (handler-bind
      ((error (lambda (&rest args) (invoke-restart (find-restart 'continue)))))
    (asdf:oos 'asdf:load-op system)))
(saveinitmem "lispinit.mem")
(quit)
EOF

if [ x$? = x0 ]; then
    gzip -f lispinit.mem
fi
