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
(clsql:def-view-class journal-entry ()
  ((id :db-kind :key
       :type integer
       :db-constraints :not-null
       :accessor id-of
       :initarg :id)
   (uuid :type (string 36)
         :db-constraints :not-null
         :accessor uuid-of
         :initarg :uuid)
   (title :type string
          :db-constraints :not-null
          :accessor title-of
          :initarg :title
          :initform "")
   (date :type universal-time
         :db-constraints :not-null
         :accessor date-of
         :initarg :date)
   (last-modification :type integer
                      :accessor last-modification-of
                      :initarg :last-modification
                      :initform nil)
   (body :type string
         :db-constraints :not-null
         :accessor body-of
         :initarg :body
         :initform "")
   (type :type string
         :db-constraints :not-null
         :accessor entry-type-of
         :initarg :type
         :initform "markdown")
   (categories :db-kind :join
               :db-constraints :not-null
               :accessor categories-of
               :initarg :categories
               :initform '()
               :db-info (:join-class journal-category
                         :home-key id
                         :foreign-key entry-id
                         :set t))
   (comments :db-kind :join
             :db-constraints :not-null
             :accessor %comments-about
             :initarg :comments
             :db-info (:join-class journal-comment
                       :home-key id
                       :foreign-key entry-id
                       :set t))
   (trackbacks :db-kind :join
               :db-constraints :not-null
               :accessor %trackbacks-about
               :initarg :trackbacks
               :db-info (:join-class journal-trackback
                         :home-key id
                         :foreign-key entry-id
                         :set t))))


(clsql:def-view-class journal-comment ()
  ((id :db-kind :key
       :type integer
       :db-constraints :not-null
       :accessor id-of
       :initarg :id)
   (entry-id :type integer
             :db-constraints :not-null
             :accessor entry-id-of
             :initarg :entry-id)
   (entry :db-kind :join
          :db-constraints :not-null
          :accessor entry-of
          :initarg :entries
          :db-info (:join-class journal-entry
                    :home-key entry-id
                    :foreign-key id
                    :set nil))
   (uuid :type (string 36)
         :db-constraints :not-null
         :accessor uuid-of
         :initarg :uuid)
   (date :type universal-time
         :db-constraints :not-null
         :accessor date-of
         :initarg :date)
   (body :type string
         :db-constraints :not-null
         :accessor body-of
         :initarg :body
         :initform "")
   (author :type string
           :accessor author-of
           :initarg :author
           :initform nil)
   (email :type string
          :accessor email-of
          :initarg :email
          :initform nil)
   (website :type string
            :accessor website-of
            :initarg :website
            :initform nil)
   (spam-p :type boolean
           :accessor spamp
           :initarg :spamp
           :initform :spamp)
   (submitter-ip :type string
                 :db-constraints :not-null
                 :accessor submitter-ip
                 :initarg :submitter-ip)
   (submitter-user-agent :type string
                         :db-constraints :not-null
                         :accessor submitter-user-agent
                         :initarg :submitter-user-agent)))

(clsql:def-view-class journal-trackback ()
  ((id :db-kind :key
       :type integer
       :db-constraints :not-null
       :accessor id-of
       :initarg :id)
   (entry-id :type integer
             :db-constraints :not-null
             :accessor entry-id-of
             :initarg :entry-id)
   (entry :db-kind :join
          :db-constraints :not-null
          :accessor entry-of
          :initarg :entries
          :db-info (:join-class journal-entry
                    :home-key entry-id
                    :foreign-key id
                    :set nil))
   (uuid :type (string 36)
         :db-constraints :not-null
         :accessor uuid-of
         :initarg :uuid)
   (date :type universal-time
         :db-constraints :not-null
         :accessor date-of
         :initarg :date)
   (excerpt :type string
            :db-constraints :not-null
            :accessor excerpt-of
            :initarg :excerpt
            :initform "")
   (title :type string
          :accessor title-of
          :initarg :title
          :initform nil)
   (blog-name :type string
              :accessor blog-name-of
              :initarg :blog-name
              :initform nil)
   (url :type string
        :accessor url-of
        :initarg :url
        :initform nil)
   (spam-p :type boolean
           :accessor spamp
           :initarg :spamp
           :initform :spamp)
   (submitter-ip :type string
                 :db-constraints :not-null
                 :accessor submitter-ip
                 :initarg :submitter-ip)
   (submitter-user-agent :type string
                         :db-constraints :not-null
                         :accessor submitter-user-agent
                         :initarg :submitter-user-agent)))


(clsql:def-view-class journal-pingback ()
  ((id :db-kind :key
       :type integer
       :db-constraints :not-null
       :accessor id-of
       :initarg :id)
   (entry-id :type integer
             :db-constraints :not-null
             :accessor entry-id-of
             :initarg :entry-id)
   (entry :db-kind :join
          :db-constraints :not-null
          :accessor entry-of
          :initarg :entries
          :db-info (:join-class journal-entry
                    :home-key entry-id
                    :foreign-key id
                    :set nil))
   (uuid :type (string 36)
         :db-constraints :not-null
         :accessor uuid-of
         :initarg :uuid)
   (date :type universal-time
         :db-constraints :not-null
         :accessor date-of
         :initarg :date)
   (url :type string
        :accessor url-of
        :initarg :url
        :initform nil)
   (spam-p :type boolean
           :accessor spamp
           :initarg :spamp
           :initform :spamp)
   (submitter-ip :type string
                 :db-constraints :not-null
                 :accessor submitter-ip
                 :initarg :submitter-ip)
   (submitter-user-agent :type string
                         :db-constraints :not-null
                         :accessor submitter-user-agent
                         :initarg :submitter-user-agent)))


(clsql:def-view-class journal-category ()
  ((id :db-kind :key
       :type integer
       :db-constraints :not-null
       :accessor id-of
       :initarg :id)
   (uuid :type (string 36)
         :db-constraints :not-null
         :accessor uuid-of
         :initarg :uuid)
   (entries :db-kind :join
            :db-constraints :not-null
            :accessor entries-in
            :initarg :entries
            :initform '()
            :db-info (:join-class journal-entry
                      :home-key id
                      :foreign-key catogory-ids
                      :set t))))


;; (@* "Journal entry operations")
(defgeneric comments-about (thing &key ordered-p))
(defgeneric (setf comments-about) (new-value thing &key ordered-p))
(defgeneric trackbacks-about (thing &key ordered-p))
(defgeneric (setf trackbacks-about) (new-value thing &key ordered-p))
(defgeneric pingbacks-about (thing &key ordered-p))
(defgeneric (setf pingbacks-about) (new-value thing &key ordered-p))

(defmethod comments-about ((journal-entry journal-entry) &key ordered-p ham-p)
  #.(locally-enable-sql-reader-syntax)
  (prog1 (if ordered-p
             (if ham-p
                 (select 'journal-comment
                         :where [and [= [slot-value 'journal-comment 'entry-id]
                                        (id-of journal-entry)]
                                     [= [slot-value 'journal-comment 'spam-p]
                                        "f"]]
                         :order-by '([date])
                         :flatp t)
                 (select 'journal-comment
                         :where [= [slot-value 'journal-comment 'entry-id]
                                   (id-of journal-entry)]
                         :order-by '([date])
                         :flatp t))
             (if ham-p
                 (comments-about journal-entry :ordered-p t :ham-p t)
                 (%comments-about journal-entry)))
    #.(restore-sql-reader-syntax-state)))


(defmethod (setf comments-about) (new-value
                                  (journal-entry journal-entry)
                                  &key ordered-p)
  (declare (ignore ordered-p))
  (setf (%comments-about journal-entry) new-value))

(defmethod trackbacks-about ((journal-entry journal-entry) &key ordered-p ham-p)
  #.(locally-enable-sql-reader-syntax)
  (prog1 (if ordered-p
             (if ham-p
                 (select 'journal-trackback
                         :where [and [= [slot-value 'journal-trackback 'entry-id]
                                        (id-of journal-entry)]
                                     [= [slot-value 'journal-trackback 'spam-p]
                                        "f"]]
                         :order-by '([date])
                         :flatp t)
                 (select 'journal-trackback
                         :where [= [slot-value 'journal-trackback 'entry-id]
                                   (id-of journal-entry)]
                         :order-by '([date])
                         :flatp t))
             (if ham-p
                 (trackbacks-about journal-entry :ordered-p t :ham-p t)
                 (%trackbacks-about journal-entry)))
    #.(restore-sql-reader-syntax-state)))

(defmethod (setf trackbacks-about) (new-value
                                    (journal-entry journal-entry)
                                    &key ordered-p)
  (declare (ignore ordered-p))
  (setf (%trackbacks-about journal-entry) new-value))


(defmethod pingbacks-about ((journal-entry journal-entry) &key ordered-p ham-p)
  #.(locally-enable-sql-reader-syntax)
  (prog1 (if ordered-p
             (if ham-p
                 (select 'journal-pingback
                         :where [and [= [slot-value 'journal-pingback 'entry-id]
                                        (id-of journal-entry)]
                                     [= [slot-value 'journal-pingback 'spam-p]
                                        "f"]]
                         :order-by '([date])
                         :flatp t)
                 (select 'journal-pingback
                         :where [= [slot-value 'journal-pingback 'entry-id]
                                   (id-of journal-entry)]
                         :order-by '([date])
                         :flatp t))
             (if ham-p
                 (pingbacks-about journal-entry :ordered-p t :ham-p t)
                 (%pingbacks-about journal-entry)))
    #.(restore-sql-reader-syntax-state)))

(defmethod (setf pingbacks-about) (new-value
                                    (journal-entry journal-entry)
                                    &key ordered-p)
  (declare (ignore ordered-p))
  (setf (%pingbacks-about journal-entry) new-value))


(defun find-largest-post-id ()
  #.(locally-enable-sql-reader-syntax)
  (prog1
    (single-object (select [max [slot-value 'journal-entry 'id]]
                           :from [journal-entry]
                           :flatp t))
    #.(restore-sql-reader-syntax-state)))


(defun make-journal-entry-id ()
  (1+ (or (find-largest-post-id) -1)))


(defun make-journal-comment-id ()
  #.(locally-enable-sql-reader-syntax)
  (prog1
      (1+ (or (single-object (select [max [slot-value 'journal-comment 'id]]
                                     :from [journal-comment]
                                     :flatp t))
              -1))
    #.(restore-sql-reader-syntax-state)))


(defun make-journal-trackback-id ()
  #.(locally-enable-sql-reader-syntax)
  (prog1
      (1+ (or (single-object (select [max [slot-value 'journal-trackback 'id]]
                                     :from [journal-trackback]
                                     :flatp t))
              -1))
    #.(restore-sql-reader-syntax-state)))


(defun make-journal-pingback-id ()
  #.(locally-enable-sql-reader-syntax)
  (prog1
      (1+ (or (single-object (select [max [slot-value 'journal-pingback 'id]]
                                     :from [journal-pingback]
                                     :flatp t))
              -1))
    #.(restore-sql-reader-syntax-state)))


(defun find-entry (number)
  #.(locally-enable-sql-reader-syntax)
  (prog1
    (single-object (select 'journal-entry
                           :where [= [slot-value 'journal-entry 'id] number]
                           :flatp t)
                   nil)
    #.(restore-sql-reader-syntax-state)))


(defun journal-markup->html (markup)
  (with-result-cache ((format nil "markup-~A" (sxhash markup))
                      :younger-than (compute-script-last-modified-date))
    (if (string= "" markup)
        markup
        (handler-bind
            ((error ;; method-call-type-error or not
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
             (markdown (remove #\Return markup)  ;<pre/> treats CR, LF as two line breaks.  Ouch.
                       :stream s)))))))


(defun compute-journal-last-modified-date ()
  #.(locally-enable-sql-reader-syntax)
  (prog1
    #-clisp (get-universal-time)
    #+clisp
    (max (compute-script-last-modified-date)
         (or (single-object
              (select [max [slot-value 'journal-entry 'last-modification]]
                      :from [journal-entry]
                      :flatp t))
             0)
         (or (single-object
              (select [max [slot-value 'journal-entry 'date]]
                      :from [journal-entry]
                      :flatp t))
             0)
         (or (single-object
              (select [max [slot-value 'journal-comment 'date]]
                      :from [journal-comment]
                      :flatp t))
             0)
         (or (single-object
              (select [max [slot-value 'journal-trackback 'date]]
                      :from [journal-trackback]
                      :flatp t))
             0))
    #.(restore-sql-reader-syntax-state)))
