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
                                                      (ext:convert-string-from-bytes
                                                       (ext:convert-string-to-bytes
                                                        (http-query-parameter param)
                                                        charset:iso-8859-1)
                                                       charset:utf-8)))
                                            (http-query-parameter-list))
                            #-clisp '())
         (*http-env*        (http-get-env-vars))
         (*subpath-query*   (subseq (gethash "REQUEST_URI" *http-env*)
                                    (length (if (eq *site* :mst-plus)
                                                (gethash "SCRIPT_NAME" *http-env*)
                                                "/journal"))))
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
                                (cond (*post-number*                      :view)
                                      ((string= "feed" (first *subpath*)) :view-atom-feed)
                                      (t                                  nil))))
         (*method*          (keywordify (gethash "REQUEST_METHOD" *http-env*)))
         (*script-filename* (pathname-as-file
                             (or (gethash "SCRIPT_FILENAME" *http-env*)
                                 "/home/mulk/Dokumente/Projekte/Mulkblog/journal.cgi")))
         (*script-dir*      (make-pathname
                             :directory (pathname-directory *script-filename*)))
         (*cache-dir*       (merge-pathnames #p"cache/" *script-dir*))
         (*entry-dir*       (merge-pathnames #p"journal-entries/" *script-dir*))
         (*journal-entries* (read-journal-entries)))
    (funcall func)))


(defmacro with-initialised-journal (&body body)
  `(call-with-initialised-journal #'(lambda () ,@body)))


#+clisp
(defun journal-main ()
  (with-initialised-journal
    (let ((*random-state* (make-random-state t)))
      (ext:letf ((custom:*terminal-encoding* (ext:make-encoding
                                              :charset charset:utf-8)))
        (case *action*
          (:post-comment   (let ((entry (find-entry *post-number*)))
                             (push (make-instance 'journal-comment
                                    :id (1+ (reduce #'max (comments-about entry)
                                                    :key #'id-of
                                                    :initial-value -1))
                                    :uuid    (make-uuid)
                                    :date    (get-universal-time)
                                    :author  (getf *query* :author)
                                    :email   (getf *query* :email)
                                    :website (getf *query* :website)
                                    :body    (getf *query* :comment-body))
                                   (comments-about entry))
                             (write-out-entry entry))
                           (show-web-journal))
          (:view-atom-feed (show-atom-feed))
          (otherwise       (show-web-journal)))))))


#+clisp
(defun cl-user::script-main ()
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
