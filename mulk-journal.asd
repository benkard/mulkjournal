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

(asdf:defsystem #:mulk-journal
  :description "Matthias Benkard's simple web journal engine"
  :licence     "Affero General Public License, version 1 or higher"
  :depends-on  (#:cl-ppcre #:cl-fad #:iterate #:cl-markdown #:parenscript
                #:yaclml #:lisp-cgi-utils #:alexandria #:xml-emitter
                #:split-sequence #:clsql #:clsql-uffi #:clsql-sqlite3
                #:drakma #:cybertiggyr-time)
  :components  ((:file "cybertiggyr-time/time.lisp")
                (:file "defpackage")
                (:file "macros")
                (:file "globals")
                (:file "utils")
                (:file "journal-content")
                (:file "journal")
                (:file "main"))
  :serial      t)
