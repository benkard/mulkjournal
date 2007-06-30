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


(defparameter *site* nil
  "One of :WIRSELKRAUT and :NFS.NET.")

(defparameter *debugging-p* nil)

(defparameter *query* nil
  "The HTTP query string transformed into a property list.")

(defparameter *http-env* nil
  "A hash table of HTTP environment variables.")

(defparameter *subpath-query* nil
  "The query string stripped of the script location.")

(defparameter *subpath-string* nil
  "The query string stripped of the script location and query parameters,
   that is, the virtual path below the script.")

(defparameter *subpath* nil
  "*SUBPATH-STRING* parsed into a list of nested directories.")

(defparameter *post-number* nil
  "The identification number of the journal entry to be acted upon.
   May be NIL.")

(defparameter *action* nil
  "One of NIL, :INDEX, :VIEW-ATOM-FEED, :VIEW, :POST, :EDIT, :PREVIEW,
   and :POST-COMMENT.")

(defparameter *method* nil
  "One of :GET, :POST, :PUT, and :DELETE.")

(defparameter *script-filename* nil)

(defparameter *journal-entries* nil
  "A list of JOURNAL-ENTRY objects.")

(defparameter *cgi-p* nil
  "Whether we have been called as a CGI script or not.")
