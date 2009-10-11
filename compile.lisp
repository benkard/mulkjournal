#! /usr/bin/env clisp
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

(in-package #:cl-user)

;;; TAKE NOTICE: If you want to run this script from the command line or
;;; from a web server, be sure to use a core image including the systems
;;; this script depends upon.  The ASDF system definition has mainly
;;; been written for purposes of debugging, development and
;;; documentation.

(defclass compile-source-simple-op (asdf:operation) ())
(defmethod asdf:perform ((o compile-source-simple-op) (c asdf:component))
  nil)
(defmethod asdf:perform ((o compile-source-simple-op) (m asdf:module))
  (dolist (c (asdf:module-components m))
    (load (compile-file (asdf:component-pathname c)))))


#+clisp
(unless (asdf:find-system :mulk-journal nil)
  (let ((*package* (find-package :asdf)))
    (load (merge-pathnames "mulk-journal.asd" system::*current-source-file*))))


;;; The following does not generally work in a CGI setting because of
;;; security restrictions.  Then again, loading all the dependencies
;;; individually rather than using a core image would certainly be too
;;; slow for any serious CGI usage, anyway, so what the heck.  Loading
;;; our own files (no dependencies) using a manually loaded system
;;; definition (see above) works, which suffices for our needs.
(unless (find-package '#:mulk.journal)
  (asdf:oos 'compile-source-simple-op '#:mulk-journal))
