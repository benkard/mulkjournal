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
           (let ((props (xml-rpc-struct-alist struct)))
             (assert (cdr (assoc :DESCRIPTION props)))
             (assert (cdr (assoc :TITLE props)))
             (create-or-edit-post (cdr (assoc :DESCRIPTION props))
                                  (cdr (assoc :TITLE props))))))
    (cond ((string= password *xml-rpc-key*) (do-stuff))
          (t (with-wsse-authentication () (do-stuff))))))


(defun mulk.journal.xml-rpc::metaWeblog.editPost (postid username password struct publish)
  (declare (ignore username publish))
  (setq postid (etypecase postid
                 (string (parse-integer postid))
                 (number postid)))
  (flet ((do-stuff ()
           (let ((props (xml-rpc-struct-alist struct)))
             (assert (cdr (assoc :DESCRIPTION props)))
             (assert (cdr (assoc :TITLE props)))
             (create-or-edit-post (cdr (assoc :DESCRIPTION props))
                                  (cdr (assoc :TITLE props))
                                  :post-id postid))))
    (cond ((string= password *xml-rpc-key*) (do-stuff))
          (t (with-wsse-authentication () (do-stuff))))))


(defun convert-entry-to-rss-item (entry)
  (with-slots (title date body categories last-modification id uuid)
              entry
     (xml-rpc-struct :CATEGORIES (map 'vector #'uuid-of categories)
                     :pubDate (xml-rpc-time date)
                     :GUID uuid
                     :POSTID (format nil "~D" id)
                     :DESCRIPTION (htmlise-entry entry)
                     :LINK (link-to :view :post-id id :absolute t)
                     :permaLink (link-to :view :post-id id :absolute t)
                     :COMMENTS (link-to :view :post-id id :absolute t)
                     :TITLE title)))


(defun mulk.journal.xml-rpc::metaWeblog.getPost (postid username password)
  (declare (ignore username password))
  (setq postid (etypecase postid
                 (string (parse-integer postid))
                 (number postid)))
  (convert-entry-to-rss-item (find-entry postid)))


(defun mulk.journal.xml-rpc::metaWeblog.getCategories (blogid username password)
  (declare (ignore blogid username password))
  #())


(defun mulk.journal.xml-rpc::metaWeblog.getRecentPosts (blogid username password number-of-posts)
  (declare (ignore blogid))
  (loop for post-id from (max 0
                              (+ (- (or (find-largest-post-id) 0)
                                    number-of-posts)
                                 1))
                    to (or (find-largest-post-id) -1)
        collect (mulk.journal.xml-rpc::metaWeblog.getPost post-id username password)))

(defun mulk.journal.xml-rpc::blogger.getUsersBlogs (appkey username password)
  (declare (ignore appkey username password))
  (list (xml-rpc-struct :BLOGID "0" :blogName "Kompottkins Weisheiten" :URL (link-to :view :absolute t))))

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
      (update-journal)
      (convert-entry-to-rss-item entry))))


(defun mulk.journal.xml-rpc::|pingback.ping| (source-uri target-uri)
  #.(locally-enable-sql-reader-syntax)
  (prog1
    (let* ((last-uri-component (first (split-sequence #\/ target-uri :from-end t :count 1)))
           (entry-id (ignore-errors (parse-integer last-uri-component)))
           (entry (and entry-id (ignore-errors (find-entry entry-id)))))
      (unless entry
        (error (make-condition 'xml-rpc-fault :code #x20 :string "Couldn't find journal entry.")))
      (with-transaction ()
        (let ((existing-pingbacks
               (select 'journal-pingback
                       :where [and [= [slot-value 'journal-pingback 'entry-id] entry-id]
                                   [= [slot-value 'journal-pingback 'url] source-uri]]
                       :flatp t)))
          (when existing-pingbacks
            (error (make-condition 'xml-rpc-fault :code #x30 :string "The pingback you wanted to do was already registered.")))
          (let ((pingback (make-instance 'journal-pingback
                             :id (make-journal-pingback-id)
                             :entry-id entry-id
                             :uuid (make-uuid)
                             :date (get-universal-time)
                             :url source-uri
                             :submitter-ip (http-getenv "REMOTE_ADDR")
                             :submitter-user-agent (http-getenv "HTTP_USER_AGENT")
                             :spamp nil)))
            (update-records-from-instance pingback)
            (update-records 'journal_pingback
                            :where [= [slot-value 'journal-pingback 'id] (id-of pingback)]
                            :av-pairs `((spam_p nil)))
            (when (eq *site* :nfs.net)
              (mail-pingback *notification-email* pingback entry))))))
    #.(restore-sql-reader-syntax-state)))
