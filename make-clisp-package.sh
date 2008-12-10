#! /bin/sh
cd ~
for x in Downloads/Git/clsql\
         Downloads/Darcs/{metatilities-base,dynamic-classes,cffi,metabang-bind,iterate,cl-markdown,cl-containers,defsystem-compatibility,alexandria,lw-compat,moptilities,metatilities,Bese/arnesi_dev,Bese/yaclml,asdf-system-connections,closer-mop,parenscript}\
         Downloads/CVS/SLIME/slime\
         .clc/site/{anaphora-0.9.3,drakma-0.6.2,xml-emitter-1.0.2,lisp-cgi-utils-0.10,cl-utilities-1.2.4}\
         /usr/share/common-lisp/source/{cl-asdf,cl-chunga,cl-plus-ssl,cl-base64,cl-fad,cl-ppcre,cl-flexi-streams,puri,split-sequence,trivial-sockets,cl-trivial-gray-streams}; do
	find "$x" -not -regex ".*/_darcs/.*" \( -regex ".*\\.c" -or -name "Makefile" -or -name "Makefile.*" -or -regex ".*\\.lisp" -or -regex ".*\\.asd" -or -regex "COPYING" -or -regex "index.lml" \)
done | tar -T - -cjf - | ssh mulk_benkard@ssh.phx.nearlyfreespeech.net 'mkdir -p /tmp/clisp-stuff && cd /tmp/clisp-stuff && tar xjf - && mkdir -p /tmp/asdf && cd /tmp/asdf && find ../clisp-stuff -name "*.asd" | xargs -I "{}" ln -sf "{}" . && mkdir -p /home/tmp/clisp-stuff/Downloads/Darcs/asdf-system-connections/website/source/images && touch /home/tmp/clisp-stuff/Downloads/Darcs/asdf-system-connections/website/source/index.lml && gmake -C /tmp/clisp-stuff/Downloads/Git/clsql/uffi' && \
ssh -t mulk_benkard@ssh.phx.nearlyfreespeech.net 'cd /tmp/clisp-stuff && clisp -x "(load \"usr/share/common-lisp/source/cl-asdf/asdf.lisp\")" -x "(let ((asdf:*central-registry* (quote (#p\"/tmp/asdf/\")))) (dolist (x (list :drakma :cffi :cl-ppcre :cl-fad :iterate :cl-markdown :parenscript :yaclml :lisp-cgi-utils :alexandria :xml-emitter :split-sequence :clsql :clsql-sqlite3)) (asdf:oos (quote asdf:load-op) x)) (saveinitmem \"lispinit.mem\"))" && gzip -f lispinit.mem && mv lispinit.mem.gz /home/private/ && mv /tmp/clisp-stuff/Downloads/Git/clsql/uffi/clsql_uffi.so /home/private/ && rm -rf /tmp/clisp-stuff && rm -rf /tmp/asdf'
