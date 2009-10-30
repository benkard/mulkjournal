;;;; -*- coding: utf-8; mode: lisp -*-
;;;; Copyright 2009, Matthias Andreas Benkard.

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

#.(setf *readtable* (copy-readtable))
#.(setf (readtable-case *readtable*) :invert)

(defun mulk.journal.xml-rpc::metaWeblog.newPost (blogid username password struct publish)
  (declare (ignore blogid username publish))
  (flet ((do-stuff ()
           (with-slots (categories pub-date guid description link comments title)
                       struct
              (create-or-edit-post description title))))
    (cond ((string= password *xml-rpc-key*) (do-stuff))
          (t (with-wsse-authentication () (do-stuff))))))


(defun mulk.journal.xml-rpc::metaWeblog.editPost (postid username password struct publish)
  (declare (ignore username publish))
  (flet ((do-stuff ()
           (with-slots (categories pub-date guid description link comments title)
                       struct
              (create-or-edit-post description title :post-id postid))))
    (cond ((string= password *xml-rpc-key*) (do-stuff))
          (t (with-wsse-authentication () (do-stuff))))))


(defun mulk.journal.xml-rpc::metaWeblog.getPost (postid username password)
  (declare (ignore username password))
  (with-slots (title date body categories last-modification id uuid)
              (find-entry postid)
     (xml-rpc-struct :CATEGORIES (mapcar #'uuid-of categories)
                     :pubDate (xml-rpc-time date)
                     :GUID uuid
                     :DESCRIPTION (htmlise-entry (find-entry postid))
                     :LINK (link-to :view :post-id postid :absolute t)
                     :COMMENTS (link-to :view :post-id postid :absolute t)
                     :TITLE title)))


(defun mulk.journal.xml-rpc::metaWeblog.getCategories (blogid username password)
  (declare (ignore blogid username password))
  #())


(defun mulk.journal.xml-rpc::metaWeblog.getRecentPosts (blogid username password number-of-posts)
  (declare (ignore blogid))
  (loop for post-id from (or (find-largest-post-id) 0) above (max 0 (- (or (find-largest-post-id) 0) number-of-posts))
        collect (mulk.journal.xml-rpc::metaWeblog.getPost post-id username password)))

(defun mulk.journal.xml-rpc::blogger.getUsersBlogs (appkey username password)
  (declare (ignore appkey username password))
  (list (xml-rpc-struct :BLOGID 0 :blogName "Kompottkins Weisheiten" :URL (link-to :view :absolute t))))

;; Not implemented: blogger.getUserInfo blogger.setTemplate blogger.getTemplate blogger.newPost blogger.editPost

(defun create-or-edit-post (body title &key entry-type post-id)
  (with-transaction ()
    (let* ((entry (if post-id
                      (find-entry post-id)
                      (make-instance 'journal-entry
                         :id (make-journal-entry-id)
                         :uuid (make-uuid)
                         :date (get-universal-time)
                         :last-modification nil
                         :categories ()
                         :comments ()))))
      (unless post-id
        (setf (last-modification-of entry)
              (get-universal-time)))
      (setf (body-of entry) (etypecase body
                              (null "")
                              (cons (xmls:toxml body :indent t))
                              (string body)))
      (setf (title-of entry) (or title ""))
      (setf (entry-type-of entry) (or entry-type "html"))
      (update-records-from-instance entry)
      ;; Update static files.
      (update-journal))))