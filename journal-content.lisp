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


;;; (@* "Class definitions")
(defclass journal-entry ()
  ((id :type (integer 0)
       :accessor id-of
       :initarg :id)
   (uuid :type string
         :accessor uuid-of
         :initarg :uuid)
   (file :type (or null pathname)
         :accessor file-of
         :initarg :file)
   (title :type string
          :accessor title-of
          :initarg :title
          :initform "")
   (date :type (integer 0)
         :accessor date-of
         :initarg :date)
   (last-modification :type (or null (integer 0))
                      :accessor last-modification-of
                      :initarg :last-modification
                      :initform nil)
   (body :type string
         :accessor body-of
         :initarg :body
         :initform "")
   (categories :type list
               :accessor categories-of
               :initarg :categories
               :initform '())
   (comments :type list
             :accessor comments-about
             :initarg :comments
             :initform '())))


(defclass journal-comment ()
  ((id :type (integer 0)
       :accessor id-of
       :initarg :id)
   (uuid :type string
         :accessor uuid-of
         :initarg :uuid)
   (date :type (integer 0)
         :accessor date-of
         :initarg :date)
   (body :type string
         :accessor body-of
         :initarg :body
         :initform "")
   (author :type (or null string)
           :accessor author-of
           :initarg :author
           :initform nil)
   (email :type (or null string)
          :accessor email-of
          :initarg :email
          :initform nil)
   (website :type (or null string)
            :accessor website-of
            :initarg :website
            :initform nil)))


;; (@* "Journal entry operations")
(defmethod shared-initialize ((journal-entry journal-entry) slot-names
                              &key)
  (with-slots (id) journal-entry
    (when (or (eq slot-names t)
              (member 'id slot-names))
      (setf id (1+ (reduce #'max *journal-entries*
                           :key #'id-of
                           :initial-value -1)))))
  (call-next-method))


(defun find-entry (number)
  (find number *journal-entries* :key #'id-of))


(defun journal-markup->html (markup)
  (if (string= "" markup)
      markup
      (handler-bind
          ((error   ;; method-call-type-error or not
            ;; Work around a weird bug in cl-markdown or CLISP.  (I
            ;; don't know which.)
            #'(lambda (c)
                (declare (ignore c))
                #+nil (<:as-html
                       (with-output-to-string (s)
                         (system::pretty-print-condition c s)))
                (invoke-restart 'return nil))))
        (fixup-markdown-output
         (with-output-to-string (s)
           ;; Normally, we shouldn't need to create our own stream to
           ;; write into, but this is, of course, yet another
           ;; CLISP/Markdown hack, because Markdown's default
           ;; *OUTPUT-STREAM* seems to spontaneously close itself, making
           ;; everything break when Markdown tries to render more stuff.
           (markdown markup :stream s))))))


(defun read-journal-entry (filename)
  (with-open-file (file filename :direction :input
                                 :external-format #+clisp charset:utf-8
                                                  #+sbcl :utf-8)
    (let ((*read-eval* nil))
      (let ((data (read file)))
        (let ((comments (member :comments data)))
          (when comments
            (setf (second comments)
                  (mapcar #'(lambda (comment-record)
                              (apply #'make-instance
                                     'journal-comment
                                     comment-record))
                          (second comments)))))
        (apply #'make-instance 'journal-entry :file filename data)))))


(defun find-journal-entry-files ()
  (let ((journal-entry-files (list)))
    (when (file-exists-p *entry-dir*)
      (walk-directory *entry-dir*
                      #'(lambda (x)
                          (push x journal-entry-files))
                      :test (complement #'directory-pathname-p)))
    journal-entry-files))


(defun read-journal-entries ()
  (let ((journal-entry-files (find-journal-entry-files)))
    (sort (mapcar #'read-journal-entry journal-entry-files)
          #'>=
          :key #'id-of)))


(defun compute-journal-last-modified-date ()
  #-clisp (get-universal-time)
  #+clisp
  (max (compute-script-last-modified-date)
       (loop for file in (find-journal-entry-files)
              maximize (posix:file-stat-mtime (posix:file-stat file)))))


(defun write-out-entry (entry)
  (assert (file-of entry))
  (with-open-file (out (file-of entry) :direction :output
                                       :if-exists :supersede
                                       :external-format #+clisp charset:utf-8
                                                        #+sbcl :utf-8)
    (with-slots (id uuid date last-modification body title categories comments)
        entry
      (write `(:id ,id
               :uuid ,uuid
               :date ,date
               :last-modification ,last-modification
               :title ,title
               :categories ,categories
               :body ,body
               :comments ,(loop for comment in comments
                             collect
                               (with-slots (id uuid date author body email
                                            website)
                                   comment
                                 `(:id ,id
                                   :uuid ,uuid
                                   :date ,date
                                   :author ,author
                                   :email ,email
                                   :website ,website
                                   :body ,body))))
             :stream out))))
