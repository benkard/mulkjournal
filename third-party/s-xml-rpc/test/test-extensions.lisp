;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-extensions.lisp,v 1.2 2006-04-19 10:22:31 scaekenberghe Exp $
;;;;
;;;; Unit and functional tests for xml-rpc.lisp
;;;;
;;;; Copyright (C) 2004 Rudi Schlatte
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-xml-rpc)

(let* ((server-port 8080)
       (server-process (start-xml-rpc-server :port server-port))
       (server-args `(:port ,server-port))
       (*xml-rpc-package* (make-package (gensym)))
       (symbols '(|system.listMethods| |system.methodSignature|
                  |system.methodHelp| |system.multicall|
                  |system.getCapabilities|)))
  (import symbols *xml-rpc-package*)
  (sleep 1)                 ; give the server some time to come up ;-)
  (unwind-protect
       (progn
         (assert
          (equal (sort (call-xml-rpc-server server-args "system.listMethods")
                       #'string<)
                 (sort (mapcar #'string symbols) #'string<)))
         (assert
          (every #'string=
                 (mapcar (lambda (name)
                           (call-xml-rpc-server server-args "system.methodHelp"
                                                name))
                         symbols)
                 (mapcar (lambda (name)
                           (or (documentation name 'function) ""))
                         symbols)))
         (assert
          (= 2
             (length (call-xml-rpc-server
                      server-args "system.multicall"
                      (list
                       (xml-rpc-struct "methodName"
                                       "system.listMethods")
                       (xml-rpc-struct "methodName"
                                       "system.methodHelp"
                                       "params"
                                       (list "system.multicall"))))))))
    (s-sysdeps:kill-process server-process)
    (delete-package *xml-rpc-package*)))

;;;; eof