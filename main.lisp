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
         (*query*           #+clisp (mapcan #'(lambda (param)
                                                (list (keywordify param)
                                                      (http-query-parameter param)))
                                            (http-query-parameter-list))
                            #-clisp '())
         (*http-env*        (http-get-env-vars))
         (*subpath-query*   (subseq (gethash "REQUEST_URI" *http-env*)
                                    (length (ecase *site*
                                              (:mst-plus (gethash "SCRIPT_NAME" *http-env*))
                                              (:nfs.net "/journal")))))
         (*subpath-string*  (subseq *subpath-query*
                                    0
                                    (or (position #\? *subpath-query*)
                                        (length *subpath-query*))))
         (*subpath*         (split-sequence #\/ *subpath-string*
                                            :remove-empty-subseqs t))
         (*post-number*     (parse-integer (or (first *subpath*)
                                               (getf *query* :id ""))
                                           :junk-allowed t  #|| :radix 12 ||#))
         (*action*          (or (keywordify (getf *query* :action))
                                (cond ((string= "feed" (first *subpath*)) :view-atom-feed)
                                      ((string= "comment-feed" (first *subpath*)) :view-comment-feed)
                                      ((string= "debug" (first *subpath*)) :view-debugging-page)
                                      ((string= "preview" (car (last *subpath*))) :preview-entry)
                                      ((string= "save" (car (last *subpath*))) :save-entry)
                                      (*post-number*                      :view)
                                      (t                                  nil))))
         (*method*          (keywordify (gethash "REQUEST_METHOD" *http-env*)))
         (*if-modified-since* #+clisp (ext:getenv "HTTP_IF_MODIFIED_SINCE")
                              #-clisp nil)
         (*script-filename* (pathname-as-file
                             (or (gethash "SCRIPT_FILENAME" *http-env*)
                                 "/home/mulk/Dokumente/Projekte/Mulkblog/journal.cgi")))
         (*script-dir*      (make-pathname
                             :directory (pathname-directory *script-filename*)))
         (*site-root*		(ecase *site*
                              (:mst-plus *script-dir*)
                              (:nfs.net
                               #+clisp (format nil "~A/" (ext:getenv "NFSN_SITE_ROOT"))
                               #-clisp (error "Don't know where to look for stuff."))))
         (*data-dir*        (ecase *site*
                              (:mst-plus *script-dir*)
                              (:nfs.net (merge-pathnames #p"protected/journal/"
                                                         *site-root*))))
         (*cache-dir*       (merge-pathnames #p"cache/" *data-dir*))
         (*wordpress-key*   (with-open-file (file (merge-pathnames
                                                   "wordpress-api-key.key"
                                                   *data-dir*))
                              (read-line file)))
         (database-file     (merge-pathnames #p"journal.sqlite3" *data-dir*))
         (sqlite-library    (merge-pathnames #p"libsqlite3.so"
                                             (ecase *site*
                                               (:mst-plus #p"/usr/lib/")
                                               (:nfs.net #p"/usr/local/lib/")))))
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
                     (update-records-from-instance entry)))
                 (show-web-journal))
    (otherwise (show-web-journal))))


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
                                  :submitter-ip (gethash "REMOTE_ADDR" *http-env*)
                                  :submitter-user-agent (gethash "HTTP_USER_AGENT" *http-env*))))
                         (push comment (comments-about entry))
                         (with-slots (spam-p) comment
                           (setq spam-p (detect-spam comment
                                                     :referrer (gethash "HTTP_REFERER" *http-env*)))
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
                           (mail-comment *notification-email* comment entry))))
                     (show-web-journal))
    (:view-atom-feed (show-atom-feed))
    (:view-comment-feed (show-comment-feed))
    (:view-debugging-page (show-debugging-page))
    (otherwise       (show-web-journal)))
  #.(restore-sql-reader-syntax-state))


#+clisp
(defun journal-main ()
  (let ((encoding (ext:make-encoding :charset charset:utf-8)))
    (ext:letf* ((custom:*terminal-encoding* encoding)
                (custom:*foreign-encoding* encoding)
                (custom:*misc-encoding* encoding)
                (custom:*pathname-encoding* encoding)
                (custom:*default-file-encoding* encoding))
      (with-initialised-journal
        (let ((*random-state* (make-random-state t)))
          (if (member "--admin-mode"
                      (coerce (ext:argv) 'list)
                      :test #'string=)
              (dispatch-admin-action)
              (dispatch-user-action)))))))


#+clisp
(defun cl-user::script-main ()
  (dolist (env-var '("HTTP_CACHE_CONTROL" "HTTP_IF_MODIFIED_SINCE"))
    (pushnew env-var http::*http-env-vars*))
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
    (journal-main)))
