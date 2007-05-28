#! /usr/bin/env clisp
;;;; -*- coding: utf-8; mode: lisp -*-
;;;; Copyright 2007, Matthias Andreas Benkard.


;;; TAKE NOTICE: If you want to run this script from the command line or
;;; from a web server, be sure to use a core image including the systems
;;; this script depends upon.  The DEFSYSTEM form below has mainly been
;;; written for purposes of documentation.
(asdf:defsystem #:mulk.journal
  :description "Matthias Benkard's simple web journal engine"
  :licence     "Affero General Public License, version 1 or higher"
  :depends-on  (#:cl-ppcre #:cl-fad #:iterate #:cl-markdown #:parenscript
                #:yaclml #:lisp-cgi-utils))


;;; The following does not generally work in a CGI setting because of
;;; security restrictions.  Loading all the dependencies individually
;;; rather than using a core image would certainly be too slow for any
;;; serious CGI usage, anyway, so what the heck.
#+nil (asdf:oos 'asdf:load-op '#:mulk.journal)


(defpackage #:mulk.journal
  (:nicknames #:journal)
  (:use #:cl #:fad #:iterate #:markdown #:yaclml #:http))

(in-package #:mulk.journal)


(defun keywordify (thing)
  (if (null thing)
      thing
      (intern (etypecase thing
                (string (string-upcase thing))
                (symbol (symbol-name   thing)))
              '#:keyword)))


(defparameter *query*
  (mapcan #'(lambda (param)
              (list (keywordify param)
                    (http-query-parameter param)))
          (http-query-parameter-list))
  "The HTTP query string transformed into a property list.")

(defparameter *action*
  (keywordify (getf *query* :action))
  "One of NIL, :INDEX, :VIEW, :POST, :EDIT, and :PREVIEW.")

(defparameter *entry-number*
  (parse-integer (getf *query* :entry "")
                 :junk-allowed t  #|| :radix 12 ||#)
  "The identification number of the blog entry to be acted upon.
   May be NIL.")

(defparameter *method*
  (keywordify (gethash "REQUEST_METHOD" (http-get-env-vars)))
  "One of :GET, :POST, :PUT, and :DELETE.")


(http-add-header "Content-type" "text/html; charset=UTF-8")
(http-send-headers)


(<:html
 (<:body
  (loop for (x . y) in `(("Action" . ,*action*)
                         ("Request method" . ,*method*)
                         ("Query" . ,*query*)
                         ("Query string" . ,(http-get-query-string))
                         ("Environment" . ,(http-get-env-vars)))
        do (<:p
            (<:hr)
            (<:h2 (<:as-html x))
            (<:p "Type " (<:em (<:as-html (type-of y))) ".")
            (<:pre (<:as-html (prin1-to-string y)))))))
