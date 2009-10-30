;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: s-xml-rpc.asd,v 1.3 2006-01-09 19:33:47 scaekenberghe Exp $
;;;;
;;;; The S-XML-RPC ASDF system definition
;;;;
;;;; Copyright (C) 2002, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :s-xml-rpc
  :name "S-XML-RPC"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "7"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>, Brian Mastenbrook <>, Rudi Schlatte <>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "Common Lisp XML-RPC Package"
  :long-description "s-xml-rpc is a Common Lisp implementation of the XML-RPC procotol for both client and server"

  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "xml-rpc" :depends-on ("package"))
                 (:file "extensions" :depends-on ("package" "xml-rpc")))))
  :depends-on (:s-xml :s-sysdeps :s-base64))

;;;; eof
