;;;; -*- coding: utf-8; mode: lisp -*-
;;;; Copyright 2007, Matthias Andreas Benkard.

;;;------------------------------------------------------------------------
;;; This file is part of The Mulkblog Project.
;;;
;;; The Mulkblog Project is free software.  You can redistribute it and/or
;;; modify it under the terms of the Affero General Public License as
;;; published by Affero, Inc.; either version 1 of the License, or
;;; (at your option) any later version.
;;;
;;; The Mulkblog Project is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the Affero General Public
;;; License in the COPYING file that comes with The Mulkblog Project; if
;;; not, write to Affero, Inc., 510 Third Street, Suite 225, San
;;; Francisco, CA 94107 USA.
;;;------------------------------------------------------------------------

(in-package #:mulk.journal)


(yaclml:deftag <xhtml (&attribute dir lang xmlns (prologue t) &body body)
  (when prologue
    (emit-princ "<?xml version='1.0' encoding='utf-8'?>")
    (emit-princ (format nil "~%"))
    (emit-princ "<!DOCTYPE html>")
    (emit-princ (format nil "~%")))
  (emit-open-tag "html" #+clisp `(("dir" . ,dir)
                                  ("lang" . ,lang)
                                  ("xml:lang" . ,lang)
                                  ("xmlns" . ,xmlns))
                        #-clisp `("dir" ,dir
                                  "lang" ,lang
                                  "xml:lang" ,lang
                                  "xmlns" ,xmlns))
  (emit-body body)
  (emit-close-tag "html"))

(yaclml::def-html-tag <::article :core :i18n :event)
(yaclml::def-html-tag <::header :core :i18n :event)
(yaclml::def-html-tag <::footer :core :i18n :event)
(yaclml::def-html-tag <::time :core :i18n :event pubdate datetime)


(defmacro with-web-journal ((page-title &key canonical-uri) &body body)
  `(call-with-web-journal ,page-title #'(lambda () ,@body) :canonical-uri ,canonical-uri))


(defmacro with-result-cache ((cache-id &key (younger-than nil younger-than-p))
                             &body body)
  `(call-with-result-cache ,cache-id
                           #'(lambda () ,@body)
                           ,@(and younger-than-p `(:younger-than ,younger-than))))


(defmacro with-initialised-journal (&body body)
  `(call-with-initialised-journal #'(lambda () ,@body)))


(defmacro regex-case (string &body clauses)
  (once-only (string)
    `(cond ,@(loop for (keys . forms) in clauses
                collect
                  `(,(if (and (symbolp keys)
                              (or (eq t keys)
                                  (equal "OTHERWISE" (symbol-name keys))))
                         't
                         `(or ,@(loop for key in (if (listp keys)
                                                     keys
                                                     (list keys))
                                   collect
                                     `(ppcre:scan-to-strings ,key ,string))))
                     ,@forms)))))

