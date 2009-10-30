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


(defun call-with-initialised-journal (func)
  (let* ((*site*            (if (file-exists-p #p"/home/mulk") :mst-plus :nfs.net))
         (*debugging-p*     (eq *site* :mst-plus))
         (*subpath-query*   (subseq (http-getenv "REQUEST_URI")
                                    (length (ecase *site*
                                              (:mst-plus (http-getenv "SCRIPT_NAME"))
                                              (:nfs.net "/journal")))))
         (*subpath-string*  (subseq *subpath-query*
                                    0
                                    (or (position #\? *subpath-query*)
                                        (length *subpath-query*))))
         (*subpath*         (split-sequence #\/ *subpath-string*
                                            :remove-empty-subseqs t))
         (*action*          (or (cond ((string= "feed" (first *subpath*)) :view-atom-feed)
                                      ((string= "comment-feed" (first *subpath*)) :view-comment-feed)
                                      ((string= "debug" (first *subpath*)) :view-debugging-page)
                                      ((string= "preview" (car (last *subpath*))) :preview-entry)
                                      ((string= "trackback" (car (last *subpath*))) :post-trackback)
                                      ((string= "save" (car (last *subpath*))) :save-entry)
                                      ((string= "moderate" (car (last *subpath*))) :moderate)
                                      ((string= "atom" (car (last *subpath*))) :view-atom-entry)
                                      ((string= "rebuild" (car (last *subpath*))) :rebuild)
                                      ((member (car (last *subpath*)) '("rpc" "RPC2") :test #'string=) :xml-rpc)
                                      (t nil))))
         (*query*           (if (eq *action* :view-atom-entry)
                                nil
                                (mapcan #'(lambda (param)
                                            (list (keywordify param)
                                                  (http-query-parameter param)))
                                        (http-query-parameter-list))))
         (*post-number*     (parse-integer (or (first *subpath*)
                                               (getf *query* :id ""))
                                           :junk-allowed t  #|| :radix 12 ||#))
         (*method*          (keywordify (http-getenv "REQUEST_METHOD")))
         (*if-modified-since* (http-getenv "HTTP_IF_MODIFIED_SINCE"))
         (*wsse* (http-getenv "HTTP_X_WSSE"))
         (*script-filename* (pathname-as-file
                             (or (http-getenv "SCRIPT_FILENAME")
                                 "/home/mulk/Dokumente/Projekte/Mulkblog/journal.cgi")))
         (*script-dir*      (make-pathname
                             :directory (pathname-directory *script-filename*)))
         (*site-root*		(ecase *site*
                              (:mst-plus *script-dir*)
                              (:nfs.net
                               (format nil "~A/" (http-getenv "NFSN_SITE_ROOT")))))
         (*data-dir*        (ecase *site*
                              (:mst-plus *script-dir*)
                              (:nfs.net (merge-pathnames #p"protected/journal/"
                                                         *site-root*))))
         (*cache-dir*       (merge-pathnames #p"cache/" *data-dir*))
         (*static-dir*      (merge-pathnames #p"public/journal/" *site-root*))
         (*wordpress-key*   (with-open-file (file (merge-pathnames
                                                   "wordpress-api-key.key"
                                                   *data-dir*))
                              (read-line file)))
         (*wsse-key*        (with-open-file (file (merge-pathnames
                                                   "wsse.key"
                                                   *data-dir*))
                              (read-line file)))
         (*xml-rpc-key*     (with-open-file (file (merge-pathnames
                                                   "xml-rpc.key"
                                                   *data-dir*))
                              (read-line file)))
         (database-file     (merge-pathnames #p"journal.sqlite3" *data-dir*))
         (sqlite-library    (merge-pathnames #p"libsqlite3.so"
                                             (ecase *site*
                                               (:mst-plus #p"/usr/lib/")
                                               (:nfs.net #p"/usr/local/lib/")))))
    (when (null *action*)
      (setq *action* (or (let ((query-action (getf *query* :action nil)))
                           (and query-action (keywordify query-action)))
                         (if *post-number*
                             (if (eq *method* :post)
                                 :post-trackback
                                 :view)
                             :index))))
    (clsql:push-library-path *script-dir*)
    (clsql:push-library-path #p"/usr/local/lib/")
    (push *script-dir* clsql-sys:*foreign-library-search-paths*)
    (clsql-uffi::load-uffi-foreign-library)
    (uffi:load-foreign-library (merge-pathnames "clsql_uffi.so"
                                                *script-dir*))
    (uffi:load-foreign-library sqlite-library)
    (clsql:with-database (db (list (namestring database-file))
                             :database-type :sqlite3
                             :make-default t)
      (assert db)
      (funcall func))))


(defun dispatch-admin-action ()
  #.(locally-enable-sql-reader-syntax)
  (case *action*
    (:preview-entry (let ((entry (and *post-number*
                                      (find-entry *post-number*))))
                      (preview-entry (or (getf *query* :title nil)
                                         (and entry (title-of entry))
                                         "")
                                     (or (getf *query* :body nil)
                                         (and entry (body-of entry))
                                         "")
                                     *post-number*)))
    (:save-entry (with-transaction ()
                   (let* ((entry (if *post-number*
                                     (find-entry *post-number*)
                                     (make-instance 'journal-entry
                                        :id (make-journal-entry-id)
                                        :uuid (make-uuid)
                                        :date (get-universal-time)
                                        :last-modification nil
                                        :categories ()
                                        :comments ()))))
                     (when *post-number*
                       (setf (last-modification-of entry)
                             (get-universal-time)))
                     (setf *post-number* (id-of entry))
                     (setf (body-of entry) (getf *query* :body)
                           (title-of entry) (getf *query* :title))
                     (update-records-from-instance entry)
                     ;; Update static files.
                     (update-index-page)
                     (update-journal-entry-page entry)
                     (update-atom-feed)))
                 (show-web-journal))
    (:moderate (let* ((id (getf *query* :id nil))
                      (type (getf *query* :type nil))
                      (acceptp (getf *query* :acceptp nil))
                      (table (if (string= type "trackback")
                                 'journal_trackback
                                 'journal_comment)))
                 (with-transaction ()
                   (when (and id type acceptp (string= acceptp "t"))
                     (update-records table
                                     :where [= [id] id]
                                     :av-pairs `((spam_p "f")))
                     ;; Update static files.
                     (update-index-page)
                     (update-comment-feed)
                     (let ((comment/trackback (single-object (select table
                                                                     :where [= [id] id]
                                                                     :flatp t))))
                       (update-journal-entry-page (entry-of comment/trackback))))
                   (when (and id type acceptp (string= acceptp "f"))
                     ;; In the negative case, there is no need to update
                     ;; any static files, as nothing will have changed
                     ;; there.
                     (update-records table
                                     :where [= [id] id]
                                     :av-pairs `((spam_p "t")))))
                 (show-moderation-page)))
    (:rebuild (http-send-headers "text/plain; charset=UTF-8")
              (update-journal)
              (format t "~&Done."))
    (otherwise (show-web-journal)))
  #.(restore-sql-reader-syntax-state))


(defun dispatch-user-action ()
  #.(locally-enable-sql-reader-syntax)
  (case *action*
    (:post-comment   (with-transaction ()
                       (let* ((entry (find-entry *post-number*))
                              (comment
                               (make-instance 'journal-comment
                                  :id       (make-journal-comment-id)
                                  :uuid     (make-uuid)
                                  :entry-id (id-of entry)
                                  :date     (get-universal-time)
                                  :author   (getf *query* :author)
                                  :email    (getf *query* :email)
                                  :website  (getf *query* :website)
                                  :body     (getf *query* :comment-body)
                                  :submitter-ip (http-getenv "REMOTE_ADDR")
                                  :submitter-user-agent (http-getenv "HTTP_USER_AGENT"))))
                         (push comment (comments-about entry))
                         (with-slots (spam-p) comment
                           (setq spam-p (detect-spam comment
                                                     :referrer (http-getenv "HTTP_REFERER")))
                           (when spam-p
                             (push (format nil
                                    "<p>Ihr Kommentar wurde als ~
                                     m&ouml;gliche unerw&uuml;nschte ~
                                     Werbung (Spam) klassifiert.  Der ~
                                     Inhaber dieses Journals wird Ihre ~
                                     Nachricht manuell moderieren ~
                                     m&uuml;ssen, weshalb eine ~
                                     Ver&ouml;ffentlichung noch etwas ~
                                     auf sich warten lassen kann.</p> ~
                                     ~
                                     <p>Wenn Sie ganz sichergehen ~
                                     wollen, da&szlig; Ihr Beitrag ~
                                     ver&ouml;ffentlicht wird, dann ~
                                     k&ouml;nnen Sie versuchen, ihn ~
                                     abzu&auml;ndern und erneut ~
                                     einzuschicken.</p>~
                                     ~
                                     <p>Hinweis: Diese Website verwendet ~
                                     <a href=\"http://akismet.com/\">Akismet</a> ~
                                     f&uuml;r die Spamerkennung.</p>")
                                     *journal-warnings*))
                           (unless spam-p
                             (push (format nil
                                    "<p>Unmoderierte Kommentierung wurde ~
                                     aufgrund des hohen Spamaufkommens ~
                                     in diesem Blog deaktiviert.  Ihr Kommentar ~
                                     wurde daher an den Betreiber des Blogs ~
                                     geschickt, welcher ihn freischalten wird, ~
                                     sobald er dazu kommt.</p>")
                                   *journal-warnings*)))
                         (update-records-from-instance comment)
                         (update-records-from-instance entry)
                         (unless (spamp comment)
                           (update-records 'journal_comment
                                           :where [= [slot-value 'journal-comment 'id] (id-of comment)]
                                           :av-pairs `((spam_p nil))))
                         (when (eq *site* :nfs.net)
                           (mail-comment *notification-email* comment entry))
                         ;; Do not update static files, as the comment
                         ;; will be waiting for moderation anyway.
                         ))
                     (show-web-journal))
    (:post-trackback (with-transaction ()
                       (let* ((entry (find-entry *post-number*))
                              (trackback
                               (make-instance 'journal-trackback
                                  :id        (make-journal-trackback-id)
                                  :uuid      (make-uuid)
                                  :entry-id  (id-of entry)
                                  :date      (get-universal-time)
                                  :blog-name (getf *query* :blog_name)
                                  :title     (getf *query* :title)
                                  :excerpt   (getf *query* :excerpt)
                                  :url       (getf *query* :url)
                                  :submitter-ip (http-getenv "REMOTE_ADDR")
                                  :submitter-user-agent (http-getenv "HTTP_USER_AGENT"))))
                         (http-send-headers "application/atom+xml; charset=UTF-8")
                         (cond
                           ((getf *query* :url)
                            (push trackback (trackbacks-about entry))
                            (with-slots (spam-p) trackback
                               (setq spam-p (detect-spam trackback
                                                         :referrer (http-getenv "HTTP_REFERER"))))
                            (update-records-from-instance trackback)
                            (update-records-from-instance entry)
                            (unless (spamp trackback)
                              (update-records 'journal_trackback
                                              :where [= [slot-value 'journal-trackback 'id] (id-of trackback)]
                                              :av-pairs `((spam_p nil))))
                            (when (eq *site* :nfs.net)
                              (mail-trackback *notification-email* trackback entry))
                            (format t "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&<response>~&<error>0</error>~&</response>"))
                           (t
                            (format t "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&<response>~&<error>1</error>~&<message>No URI was provided.</message>~&</response>")))
                         ;; Update static files.
                         (update-comment-feed)
                         (update-journal-entry-page entry)
                         (update-index-page))))
    (:view-atom-entry
     (with-wsse-authentication ()
       (cond ((eq *method* :get)
              (if *post-number*
                  (show-atom-entry)
                  (show-atom-feed :include-edit-links t :full-content t)))
             ((member *method* '(:post :put))
              (with-transaction ()
                (let* ((entry (if (string= *method* "PUT")
                                  (find-entry *post-number*)
                                  (make-instance 'journal-entry
                                     :id (make-journal-entry-id)
                                     :uuid (make-uuid)
                                     :date (get-universal-time)
                                     :last-modification nil
                                     :categories ()
                                     :comments ()))))
                  (when (string= *method* "PUT")
                    (setf (last-modification-of entry)
                          (get-universal-time)))
                  (setf *post-number* (id-of entry))
                  (flet ((tag-equal (tag1 tag2)
                           (equal (if (consp tag1) (car tag1) tag1)
                                  (if (consp tag2) (car tag2) tag2))))
                    (let* ((post-data (slurp-post-data))
                           (xml (xmls:parse post-data))
                           (entry-elements (cddr xml))
                           (content-element (find "content" entry-elements :key 'car :test #'tag-equal))
                           (content (caddr content-element))
                           (title-element (find "title" entry-elements :key 'car :test #'tag-equal))
                           (title-type (cadr (assoc "type" (cadr title-element))))
                           (content-type (cadr (assoc "type" (cadr content-element))))
                           (content-mode (cadr (assoc "type" (cadr content-element)))))
                      (when content-element
                        (setf (body-of entry) (etypecase content
                                                (null "")
                                                (cons (xmls:toxml content :indent t))
                                                (string content))))
                      (when title-element
                        (setf (title-of entry) (or (caddr title-element) "")))
                      (setf (entry-type-of entry) "html")))
                  (update-records-from-instance entry)
                  ;; Update static files.
                  (update-journal)))
              (show-atom-entry))
             (t (debug-log "Oops. Method was:") (debug-log *method*)))))
    (:view-atom-feed (show-atom-feed))
    (:view-comment-feed (show-comment-feed))
    (:view-debugging-page (show-debugging-page))
    (:xml-rpc (when (eq *method* :post)
                (let ((xml-data (slurp-post-data)))
                  (http-add-header "Content-Language" "de")
                  (http-send-headers "text/xml; charset=UTF-8")
                  (write (let ((*xml-rpc-package*
                                (find-package '#:mulk.journal.xml-rpc)))
                           (s-xml-rpc::handle-xml-rpc-call xml-data 0))
                         :stream *standard-output*))))
    (otherwise (show-web-journal)))
  #.(restore-sql-reader-syntax-state))


(defun slurp-post-data ()
  (with-output-to-string (out)
    (loop for line = (read-line *standard-input* nil nil nil)
          while line
          do (write-line line out))))

#+clisp
(defun journal-main (&key admin-mode)
  (let ((encoding (ext:make-encoding :charset charset:utf-8)))
    (ext:letf* ((custom:*terminal-encoding* encoding)
                (custom:*foreign-encoding* encoding)
                (custom:*misc-encoding* encoding)
                (custom:*pathname-encoding* encoding)
                (custom:*default-file-encoding* encoding))
      (with-initialised-journal
        (let ((*random-state* (make-random-state t)))
          (if admin-mode
              (dispatch-admin-action)
              (dispatch-user-action)))))))


#+clisp
(defun cl-user::script-main (&key admin-mode)
  (http:http-init)
  (setq cffi:*default-foreign-encoding* :iso-8859-15)
  (handler-bind
      ((error #'
        (lambda (e)
          (declare (ignorable e))
          (<:html
           (<:head
            (<:title "Kompottkins Weisheiten: Fehler"))
           (<:body
            (<:h1 "Kompottkins Weisheiten: Fehlerbericht")
            (<:p "Leider ist waehrend der Bearbeitung Ihrer Anfrage ein
                Fehler aufgetreten.  Wir bitten dies zu entschuldigen.
                Ein detaillierter Fehlerbericht folgt.")
            (<:pre (<:as-html (with-output-to-string (out)
                                #+clisp (system::pretty-print-condition e out)
                                #+clisp (system::print-backtrace :out out)))))))))
    (journal-main :admin-mode (member "--admin-mode"
                                      (coerce (ext:argv) 'list)
                                      :test #'string=))))
