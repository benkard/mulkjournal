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


(defun link-to (action &key post-id (absolute nil))
  (with-output-to-string (out)
    (format out "~A" (if absolute
                         "http://matthias.benkard.de/journal"
                         ;; When testing on the local webserver, don't
                         ;; use /journal as a relative URI, because it
                         ;; won't work.
                         (if (eq *site* :mst-plus)
                             (gethash "SCRIPT_NAME" *http-env* "")
                             "/journal")))
    (multiple-value-call
        #'(lambda (&rest args) (apply #'format out args))
      (case action
        (:index "")
        (:view-atom-feed (values "/feed"))
        (:view (values "/~D" post-id))
        (:edit (values "/~D?action=edit" post-id))
        (:post-comment (values "/~D" post-id))
        (:css (if (eq *site* :mst-plus)
                  "/../../journal.css"
                  "/../journal.css"))))))


(defun show-atom-feed ()
  (http-add-header "Last-Modified" (http-timestamp (compute-journal-last-modified-date)))
  (http-send-headers "application/atom+xml; charset=UTF-8")

  (flet ((atom-time (time)
           (format-date nil
                        "%4yr-%2mon-%2dayT%2hr:%2min:%2secZ"
                        time
                        0)))
    (with-xml-output (*standard-output* :encoding "utf-8")
      (with-tag ("feed" '(("xmlns" "http://www.w3.org/2005/Atom")))
        (emit-simple-tags :title "Kompottkins Weisheiten"
                          :updated (atom-time
                                    (max (reduce #'max *journal-entries*
                                                 :key #'date-of
                                                 :initial-value 0)
                                         (reduce #'(lambda (x y)
                                                     (cond ((and x y)
                                                            (max x y))
                                                           (x x)
                                                           (y y)
                                                           (t 0)))
                                                 *journal-entries*
                                                 :key #'last-modification-of
                                                 :initial-value 0)))
                          :id "urn:uuid:88ad4730-90bc-4cc1-9e1f-d4cdb9ce177c")
        (with-tag ("subtitle")
          (xml-as-is "Geschwafel eines libert&#xE4;rsozialistischen Geeks"))
        (with-tag ("author")
          (emit-simple-tags :name "Matthias Benkard"))
        (with-tag ("link" `(("rel" "alternate")
                            ("type" "text/html")
                            ("href" ,(link-to :index :absolute t)))))
        (with-tag ("link" `(("rel" "self")
                            ("type" "application/atom+xml")
                            ("href" ,(link-to :view-atom-feed :absolute t)))))

        (dolist (journal-entry (sort (copy-list *journal-entries*)
                                     #'>
                                     :key #'date-of))
          (with-slots (title date body categories last-modification id)
              journal-entry
            (with-tag ("entry")
              (emit-simple-tags :title title
                                :id (format nil "urn:uuid:~(~A~)"
                                            (uuid-of journal-entry))
                                :updated (atom-time (or last-modification date))
                                :published (atom-time date))
              (with-tag ("link" `(("rel" "alternate")
                                  ("type" "text/html")
                                  ("href" ,(link-to :view
                                                    :post-id id
                                                    :absolute t)))))
              (with-tag ("content" `(("type" "xhtml")
                                     ("xml:lang" "de")
                                     ("xml:base" ,(link-to :index :absolute t))))
                (with-tag ("div" '(("xmlns" "http://www.w3.org/1999/xhtml")))
                  (xml-as-is (journal-markup->html (body-of journal-entry))))))))))))


(let ((scanner (ppcre:create-scanner "(\\n|\\r|\\r\\n)(\\n|\\r|\\r\\n)+")))
  (defun render-comment-body (text)
    (loop for last-position = 0 then (cadr matches)
          for matches = (ppcre:all-matches scanner
                                           text)
          then (cddr matches)
          while (not (endp matches))
          do (<:p (<:as-html (subseq text last-position (car matches))))
          finally
            (<:p (<:as-html (subseq text last-position))))))


(defun show-journal-entry (journal-entry &key (comments-p nil))
  (<:div :class :journal-entry
   (<:h2 (<:a :href (link-to :view :post-id (id-of journal-entry))
              (<:as-html (title-of journal-entry))))
    (<:div :class :journal-entry-header
     (<:span :class :journal-entry-date
      (<:as-html
       (format-date nil "%@day-of-week, den %day.%mon.%yr, %hr:%2min."
                    (date-of journal-entry))))
     (unless (null (categories-of journal-entry))
       (<:span :class :journal-entry-category
        (<:as-html
         (format nil "Abgeheftet unter ...")))))
    (<:div :class :journal-entry-body
     (<:as-is (journal-markup->html (body-of journal-entry))))
    (<:div :class :journal-entry-footer
     (<:form :class :journal-entry-delete-button-form
             :style "display: inline;"
             :method "post"
             :action (link-to :index)
      (<:div :style "display: inline;"
       (<:input :type "hidden"
                :name "action"
                :value "delete")
       (<:input :type "hidden"
                :name "id"
                :value (prin1-to-string (id-of journal-entry)))
       (<:button :type "submit"
                 (<:as-is "L&ouml;schen"))))
     " | "
     (<:form :class :journal-entry-delete-button-form
             :style "display: inline;"
             :method "get"
             :action (link-to :index)
      (<:div :style "display: inline;"
       (<:input :type "hidden"
                :name "action"
                :value "edit")
       (<:input :type "hidden"
                :name "id"
                :value (prin1-to-string (id-of journal-entry)))
       (<:button :type "submit"
                 (<:as-is "Bearbeiten"))))
     " | "
     (<:a :href (link-to :view :post-id (id-of journal-entry))
          (<:as-is
           (format nil "~D Kommentar~:*~[e~;~:;e~]" (length (comments-about journal-entry)))))))

  (when (and comments-p (not (null (comments-about journal-entry))))
    (<:div :class :journal-comments
     (<:h2 "Kommentare")
     (dolist (comment (sort (copy-list (comments-about journal-entry))
                            #'<
                            :key #'date-of))
       (with-slots (author body date id email website)
           comment
         (<:div :class :journal-comment
          (<:div :class :journal-comment-header
           (<:as-html (format nil "(~A) "
                              (format-date nil "%day.%mon.%yr, %hr:%min" date)))
           (<:a :href website
            (<:as-html (format nil "~A" author)))
           (<:as-html " meint: "))
          (<:div :class :journal-comment-body
           (<:as-html (render-comment-body body))))))))

  (when comments-p
    (<:div :class :journal-new-comment
     (<:h2 "Neuen Kommentar schreiben")
     (<:p (<:as-is "Bitte beachten Sie, da&szlig; E-Mail-Adressen niemals
                    ver&ouml;ffentlicht werden und nur von Matthias eingesehen
                    werden k&ouml;nnen."))
     (<:form :action (link-to :view :post-id (id-of journal-entry))
             :method "post"
             :accept-charset "UTF-8"
      (<:div :style "display: hidden"
       (<:input :type "hidden"
                :name "id"
                :value (prin1-to-string (id-of journal-entry)))
       (<:input :type "hidden"
                :name "action"
                :value "post-comment"))
      (<:div :style "display: table"
       (loop for (name . desc) in '(("author" . "Name (n&ouml;tig)")
                                    ("email" . "E-Mail")
                                    ("website" . "Website"))
             do (<:div :style "display: table-row"
                 (<:div :style "display: table-cell; vertical-align: top"
                  (<:label :for name
                           :style "vertical-align: top"
                   (<:as-is (format nil "~A: " desc))))
                 (<:div :style "display: table-cell;"
                  (<:input :type "text"
                           :name name
                           :id name))))
       (<:div :style "display: table-row"
        (<:div :style "display: table-cell; vertical-align: top"
         (<:label :for "comment-body"
                  :style "vertical-align: top"
          (<:as-html "Kommentar: ")))
        (<:div :style "display: table-cell"
         (<:textarea :name "comment-body"
                     :id "comment-body"
                     :rows 10
                     :cols 40))))
      (<:div
       (<:button :type "submit"
        (<:as-is "Ver&ouml;ffentlichen")))))))


(defun show-web-journal ()
  (http-add-header "Last-Modified" (http-timestamp (compute-journal-last-modified-date)))
  (http-send-headers "text/html; charset=UTF-8")

  (<xhtml :xmlns "http://www.w3.org/1999/xhtml"
   (<:head
    (<:title
     (<:as-html
      (if (member *action* '(:view :edit :preview :post-comment))
          (format nil "~A -- Kompottkins Weisheiten"
                  (title-of (find-entry *post-number*)))
          "Kompottkins Weisheiten")))
    (<:link :rel "alternate"
            :type "application/atom+xml"
            :href (link-to :view-atom-feed)
            :title "Kompottkins weiser Atom-Feed")
    (<:link :rel "stylesheet" :type "text/css" :href (link-to :css))
    (<:link :rel "openid.server" :href "https://meinguter.name/index.php/serve")
    (<:link :rel "openid.delegate" :href "https://matthias.benkard.meinguter.name"))
   (<:body
    (<:div :id :main-title-box
     (<:h1 :id :main-title
           (<:a :href (link-to :index)
                "Kompottkins Weisheiten"))
     (<:div :id :main-subtitle
      (<:as-is "&bull;&bull;&bull; ")
      (<:as-is
       (random-elt
        '("Geschwafel eines libert&auml;rsozialistischen Geeks"
          "NEU!  Jetzt ohne regelm&auml;&szlig;ige Serverabst&uuml;rze!"
          "NEU!  Jetzt mit mehr als 3 % Uptime!")))
      (<:as-is " &bull;&bull;&bull;")))
    (<:div :id :contents
     (case *action*
       ((:index nil)
        (mapc #'show-journal-entry (sort (copy-list *journal-entries*)
                                         #'>
                                         :key #'date-of)))
       ((:view :post-comment)
        (show-journal-entry (find-entry *post-number*) :comments-p t))))
    (<:div :id :navigation))


    (when *debugging-p*
      (loop for (x . y) in `(("Action" . ,*action*)
                             ("Request method" . ,*method*)
                             ("Query" . ,*query*)
                             ("Query string" . ,(http-get-query-string))
                             ("Subpath" . ,*subpath*)
                             ("Environment" . ,(http-get-env-vars)))
         do (<:p
             (<:hr)
             (<:h2 (<:as-html x))
             (<:p "Type " (<:em (<:as-html (type-of y))) ".")
             (<:pre (<:as-html (prin1-to-string y))))))))

