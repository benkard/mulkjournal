;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-xml-rpc.lisp,v 1.4 2008-02-15 15:42:40 scaekenberghe Exp $
;;;;
;;;; Unit and functional tests for xml-rpc.lisp
;;;;
;;;; Copyright (C) 2002, 2004 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :s-xml-rpc)

(assert
 (let ((now (get-universal-time)))
   (equal (iso8601->universal-time (universal-time->iso8601 now))
	  now)))

(assert
 (equal (with-input-from-string (in (encode-xml-rpc-call "add" 1 2))
	  (decode-xml-rpc in))
	'("add" 1 2)))

(assert
 (equal (with-input-from-string (in (encode-xml-rpc-result '(1 2)))
	  (car (decode-xml-rpc in)))
	'(1 2)))

(let ((condition (with-input-from-string (in (encode-xml-rpc-fault "Fatal Error" 100))
		   (decode-xml-rpc in))))
  (assert (typep condition 'xml-rpc-fault))
  (assert (equal (xml-rpc-fault-string condition) "Fatal Error"))
  (assert (equal (xml-rpc-fault-code condition) 100)))

(assert
 (xml-rpc-time-p (xml-rpc-call (encode-xml-rpc-call "currentTime.getCurrentTime")
			       :host "time.xmlrpc.com")))

(assert
 (equal (xml-rpc-call (encode-xml-rpc-call "examples.getStateName" 41)
		      :host "betty.userland.com")
	"South Dakota"))

(assert
 (equal (call-xml-rpc-server '(:host "betty.userland.com") "examples.getStateName" 41)
	"South Dakota"))

#-clisp
(assert
 (let ((server-process (start-xml-rpc-server :port 8080)))
   (import 's-xml-rpc::xml-rpc-implementation-version :s-xml-rpc-exports)
   (sleep 1) ; give the server some time to come up ;-)
   (unwind-protect
       (equal (xml-rpc-call (encode-xml-rpc-call "XML-RPC-IMPLEMENTATION-VERSION") :port 8080)
	      (xml-rpc-implementation-version))
     (s-sysdeps:kill-process server-process)
     (unintern 's-xml-rpc::xml-rpc-implementation-version :s-xml-rpc-exports))))

(assert
    (let* ((struct-in (xml-rpc-struct :foo 100 :bar ""))
           (xml (with-output-to-string (out)
                  (encode-xml-rpc-value struct-in out)))
           (struct-out (with-input-from-string (in xml)
                         (decode-xml-rpc in))))
      (xml-rpc-struct-equal struct-in struct-out)))

;; testing whitespace handling

(assert (null (decode-xml-rpc (make-string-input-stream 
"<array>
  <data>  
  </data>
</array>")))) 

(assert (equalp (decode-xml-rpc (make-string-input-stream
"<params>
  <param>
    <value>
       foo
    </value>
  </param>
  <param>
    <value>
      <array>
        <data>
          <value><i4>12</i4></value>
          <value><string>Egypt</string></value>
          <value><boolean>1</boolean></value>
          <value> <string>      </string> </value>
          <value>   </value>
          <value> fgo </value>
          <value><i4>-31</i4></value>
          <value></value>
          <double>		-12.214 </double>
          <dateTime.iso8601>
                 19980717T14:08:55 </dateTime.iso8601>
          <base64>eW91IGNhbid0IHJlYWQgdGhpcyE=</base64>
        </data>
      </array>
    </value>
  </param>
</params>"))
`("
       foo
    " 
  (12 
   "Egypt" 
   T 
   "      " 
   "   " 
   " fgo " 
   -31 
   "" 
   -12.214D0 
   ,(xml-rpc-time (iso8601->universal-time  "19980717T14:08:55")) 
   #(121 111 117 32 99 97 110 39 116 32 114 101 97 100 32 116 104 105 115 33)))))

(assert (equalp (decode-xml-rpc (make-string-input-stream 
"<array>
  <data>
    <value></value>
  </data>
</array>"))
'("")))

(assert (equalp (decode-xml-rpc (make-string-input-stream 
"<array>
  <data>
    <value>
      <string>XYZ</string>
    </value>
  </data>
</array>"))
'("XYZ")))

;; double decoding

(assert (< (abs (- (decode-xml-rpc (make-string-input-stream "<value><double>3.141592653589793</double></value>"))
                   pi))
           0.000000000001D0))

;; string decoding

(assert (equal (decode-xml-rpc (make-string-input-stream "<value><string>foo</string></value>"))
               "foo"))

(assert (equal (decode-xml-rpc (make-string-input-stream "<value>foo</value>"))
               "foo"))

(assert (equal (decode-xml-rpc (make-string-input-stream "<value><string></string></value>"))
               ""))

(assert (equal (decode-xml-rpc (make-string-input-stream "<value></value>"))
               ""))

;; boolean encoding

(assert (equal (with-output-to-string (out)
                 (encode-xml-rpc-value t out))
               "<value><boolean>1</boolean></value>"))

(assert (equal (with-output-to-string (out)
                 (encode-xml-rpc-value nil out))
               "<value><boolean>0</boolean></value>"))


;; boolean decoding

(assert (equal (decode-xml-rpc (make-string-input-stream "<value><boolean>1</boolean></value>"))
               t))

(assert (equal (decode-xml-rpc (make-string-input-stream "<value><boolean>0</boolean></value>"))
               nil))

;;;; eof
