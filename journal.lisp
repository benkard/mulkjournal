;;;; -*- coding: utf-8; mode: lisp -*-
;;;; Copyright 2007-2009, Matthias Andreas Benkard.

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


(defun link-to (action &key comment-id post-id (absolute nil))
  (with-output-to-string (out)
    (format out "~A" (if absolute
                         "http://matthias.benkard.de/journal"
                         ;; When testing on the local webserver, don't
                         ;; use /journal as a relative URI, because it
                         ;; won't work.
                         (if (eq *site* :mst-plus)
                             (http-getenv "SCRIPT_NAME")
                             "/journal")))
    (multiple-value-call
        #'(lambda (&rest args) (apply #'format out args))
      (ecase action
        (:index "")
        (:full-index "/?full")
        (:view-atom-feed (values "/feed"))
        (:view-comment-feed (values "/comment-feed"))
        (:view (cond (comment-id (values "/~D#comment-~D" post-id comment-id))
                     (post-id (values "/~D" post-id))
                     (t "/")))
        ((:edit :preview) (values "/~D/preview" post-id))
        (:post-comment (values "/~D" post-id))
        (:trackback (values "/~D/trackback" post-id))
        (:view-atom-entry (values "/~D/atom" post-id))
        (:save (values "/~D/save" post-id))
        (:moderation-page "/moderate")
        (:css "/../journal.css")
        (:pingback "/rpc")))))


(defun show-comment-feed ()
  #.(locally-enable-sql-reader-syntax)
  (revalidate-cache-or-die "application/atom+xml; charset=UTF-8")
  (when (eq *mode* :http)
    (http-add-header "Last-Modified" (http-timestamp (compute-journal-last-modified-date)))
    (http-add-header "Content-Language" "de")
    (http-send-headers "application/atom+xml; charset=UTF-8"))

  (flet ((atom-time (time)
           (format-date nil
                        "%4yr%-%2mon%-%2day%T%2hr%:%2min%:%2sec%Z"
                        time
                        0)))
    (with-xml-output (*standard-output* :encoding "utf-8")
      (with-tag ("feed" '(("xmlns" "http://www.w3.org/2005/Atom")))
        (with-tag ("title")
          (xml-as-is "Kommentare &#8212; Kompottkins Weisheiten"))
        (emit-simple-tags :updated (atom-time
                                    (max (or (single-object
                                               (select [max [slot-value 'journal-entry 'date]]
                                                       :from [journal-entry]
                                                       :flatp t))
                                             0)
                                         (or (single-object
                                               (select [max [slot-value 'journal-entry 'last-modification]]
                                                       :from [journal-entry]
                                                       :flatp t))
                                             0)))
                          :id "urn:uuid:9cd7a24c-10a6-4895-a97b-8df6b426e4a0")
        (with-tag ("subtitle")
          (xml-as-is "Geschwafel zum Geschwafel eines libert&#xE4;rsozialistischen Geeks"))
        (with-tag ("author")
          (emit-simple-tags :name "Various"))
        (with-tag ("link" `(("rel" "alternate")
                            ("type" "text/html")
                            ("href" ,(link-to :index :absolute t)))))
        (with-tag ("link" `(("rel" "self")
                            ("type" "application/atom+xml")
                            ("href" ,(link-to :view-comment-feed :absolute t)))))

        (let ((number 0))
          (dolist (journal-comment (select 'journal-comment
                                           :where [= [slot-value 'journal-comment 'spam-p] "f"]
                                           :order-by '(([date] :desc))
                                           :flatp t))
            (with-slots (entry uuid date body author website spam-p id)
                        journal-comment
               (unless spam-p
                 (incf number)
                 (with-tag ("entry")
                   (emit-simple-tags :title (format nil "Kommentar zu: ~A" (title-of entry))
                                     :id (format nil "urn:uuid:~(~A~)" uuid)
                                     :updated (atom-time date)
                                     :published (atom-time date))
                   (with-tag ("link" `(("rel" "alternate")
                                       ("type" "text/html")
                                       ("href" ,(link-to :view
                                                         :comment-id id
                                                         :post-id (id-of entry)
                                                         :absolute t)))))
                   (when (<= number 8)
                     ;; We only include the body for the most recent
                     ;; posts in order to save bandwidth.
                     (with-tag ("content" `(("type" "xhtml")
                                            ("xml:lang" "de")
                                            ("xml:base" ,(link-to :index :absolute t))))
                       (with-tag ("div" '(("xmlns" "http://www.w3.org/1999/xhtml")))
                         (xml-as-is
                          (with-yaclml-output-to-string
                            (<:as-html
                             (render-comment-body body)))))))))))))))
  #.(restore-sql-reader-syntax-state))


(defun show-atom-entry ()
  #.(locally-enable-sql-reader-syntax)
  (revalidate-cache-or-die "application/atom+xml; charset=UTF-8")
  (when (eq *mode* :http)
    (http-add-header "Last-Modified" (http-timestamp (compute-journal-last-modified-date)))
    (http-add-header "Content-Language" "de")
    (http-send-headers "application/atom+xml; charset=UTF-8"))

  (with-xml-output (*standard-output* :encoding "utf-8")
    (show-atom-entry-xml (find-entry *post-number*) :full-content t :include-edit-links t)))


(defun show-atom-entry-xml (journal-entry &key full-content include-edit-links)
  (flet ((atom-time (time)
           (format-date nil
                        "%4yr%-%2mon%-%2day%T%2hr%:%2min%:%2sec%Z"
                        time
                        0)))
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
         (when include-edit-links
           (with-tag ("link" `(("rel" "service.edit")
                               ("type" "application/atom+xml")
                               ("href" ,(link-to :view-atom-entry
                                                 :post-id id
                                                 :absolute t))
                               ("title" ,title)))))
         (when full-content
           ;; Escaped HTML or embedded XHTML?  What shall we prefer?
           #+(or)
           (with-tag ("content" `(("type" "html")
                                  ("xml:lang" "de")
                                  ("xml:base" ,(link-to :index :absolute t))))
             (xml-out (if (equal (entry-type-of journal-entry) "html")
                          (body-of journal-entry)
                          (journal-markup->html (body-of journal-entry)))))
           (with-tag ("content" `(("type" "xhtml")
                                  ("xml:lang" "de")
                                  ("xml:base" ,(link-to :index :absolute t))))
             (with-tag ("div" '(("xmlns" "http://www.w3.org/1999/xhtml")))
               (xml-as-is (htmlise-entry journal-entry))))))))
  #.(restore-sql-reader-syntax-state))


(defun htmlise-entry (journal-entry)
  (if (equal (entry-type-of journal-entry) "html")
      (body-of journal-entry)
      (journal-markup->html (body-of journal-entry))))


(defun show-atom-feed (&key include-edit-links full-content)
  #.(locally-enable-sql-reader-syntax)
  (revalidate-cache-or-die "application/atom+xml; charset=UTF-8")
  (when (eq *mode* :http)
    (http-add-header "Last-Modified" (http-timestamp (compute-journal-last-modified-date)))
    (http-add-header "Content-Language" "de")
    (http-send-headers "application/atom+xml; charset=UTF-8"))

  (flet ((atom-time (time)
           (format-date nil
                        "%4yr%-%2mon%-%2day%T%2hr%:%2min%:%2sec%Z"
                        time
                        0)))
    (with-xml-output (*standard-output* :encoding "utf-8")
      (with-tag ("feed" '(("xmlns" "http://www.w3.org/2005/Atom")))
        (emit-simple-tags :title "Kompottkins Weisheiten"
                          :updated (atom-time
                                    (max (or (single-object
                                               (select [max [slot-value 'journal-entry 'date]]
                                                       :from [journal-entry]
                                                       :flatp t))
                                             0)
                                         (or (single-object
                                               (select [max [slot-value 'journal-entry 'last-modification]]
                                                       :from [journal-entry]
                                                       :flatp t))
                                             0)))
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
        (when include-edit-links
          (with-tag ("link" `(("rel" "service.post")
                              ("type" "application/atom+xml")
                              ("href" ,(link-to :view-atom-entry :absolute t)))))
          (with-tag ("link" `(("rel" "service.feed")
                              ("type" "application/atom+xml")
                              ("href" ,(link-to :view-atom-entry :absolute t)))))
          #+(or) (with-tag ("link" `(("rel" "service.categories")
                                     ("type" "application/atom+xml")
                                     ("href" ,(link-to :view-atom-entry :absolute t))))))

        (let ((number 0))
          (dolist (journal-entry (select 'journal-entry
                                         :order-by '(([date] :desc))
                                         :flatp t))
            ;; We only include the body for the most recent posts in
            ;; order to save bandwidth.
            (show-atom-entry-xml journal-entry
                                 :full-content (or (and (last-modification-of journal-entry)
                                                        (> (last-modification-of journal-entry)
                                                           (- (get-universal-time)
                                                              (* 30 24 60 60))))
                                                   (<= number 8)
                                                   full-content)
                                 :include-edit-links include-edit-links)
            (incf number))))))
  #.(restore-sql-reader-syntax-state))


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
  (with-slots (id title body categories date type) journal-entry
     (show-journal-entry-with-components id title body categories date type
                                         (comments-about journal-entry
                                                         :ordered-p t
                                                         :ham-p t)
                                         comments-p
                                         (trackbacks-about journal-entry
                                                           :ordered-p t
                                                           :ham-p t))))


(defun show-journal-entry-with-components (id title body categories posting-date
                                           type comments comments-p trackbacks)
  (unless *full-entry-view*
    (<:tr
     (<:td (<:a :href (link-to :view :post-id id)
            (<:as-is title)))
     (<:td :style "text-align: right"
      (<:as-is (format-date nil "%day%.%mon%.%yr%,&nbsp;%hr%:%2min%" posting-date)))
     (<:td (<:a :href (link-to :view :post-id id)
            (<:as-is
             (format nil "~D&nbsp;Kommentar~:*~[e~;~:;e~]" (length comments)))))))

  (when *full-entry-view*
    (<:div :class :journal-entry
     (<:h2 (<:a :href (link-to :view :post-id id)
            (<:as-is title)))
     (<:div :class :journal-entry-header
      (<:span :class :journal-entry-date
       (<:as-html
        (format-date nil "%@day-of-week%, den %day%.%mon%.%yr%, %hr%:%2min%."
                     posting-date)))
      (unless (null categories)
        (<:span :class :journal-entry-category
         (<:as-html
          (format nil "Abgeheftet unter ...")))))
      (<:div :class :journal-entry-body
       (<:as-is (if (equal type "html")
                    body
                    (journal-markup->html body))))
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
                  :value (prin1-to-string id))
         (<:button :type "submit"
                   (<:as-is "L&ouml;schen"))))
       " | "
       (<:form :class :journal-entry-edit-button-form
               :style "display: inline;"
               :method "get"
               :action (link-to :edit :post-id id)
        (<:div :style "display: inline;"
         (<:input :type "hidden"
                  :name "id"
                  :value (prin1-to-string id))
         (<:button :type "submit"
                   (<:as-is "Bearbeiten"))))
       " | "
       (<:a :href (link-to :view :post-id id)
        (<:as-is
         (format nil "~D Kommentar~:*~[e~;~:;e~]" (length comments))))))

    (when (and comments-p (not (null comments)))
      (<:div :class :journal-comments
       (<:h2 "Kommentare")
       (dolist (comment comments)
         (show-comment comment))))

    (when (and comments-p (not (null trackbacks)))
      (<:div :class :journal-comments
       (<:h2 "Trackbacks")
       (dolist (trackback trackbacks)
         (show-trackback trackback))))

    (when comments-p
      (<:as-is (format nil "<!--
    <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
             xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
             xmlns:trackback=\"http://madskills.com/public/xml/rss/module/trackback/\">
    <rdf:Description
        rdf:about=\"~A\"
        dc:identifier=\"~:*~A\"
        dc:title=\"~A\"
        trackback:ping=\"~A\" />
    </rdf:RDF>
-->" (link-to :view :post-id id :absolute t) (ppcre:regex-replace "--" title "&#8212;") (link-to :trackback :post-id id :absolute t)))
      (<:div :class :journal-new-comment
       (<:h2 "Neuen Kommentar schreiben")
       (<:p (<:as-is "Bitte beachten Sie, da&szlig; E-Mail-Adressen niemals
                      ver&ouml;ffentlicht werden und nur von Matthias eingesehen
                      werden k&ouml;nnen."))
       (<:p (<:strong "Hinweise: ")
            "Diese Website verwendet "
            (<:a :href "http://akismet.com/" "Akismet")
            " zur Spamerkennung. "
            (<:as-is "E-Mail-Adressen werden auch gegen&uuml;ber Akismet
                      unter Verschlu&szlig; gehalten.  Nur unformatierter
                      Text ist erlaubt.  Leerzeilen trennen
                      Abs&auml;tze."))
       (<:form :action (link-to :view :post-id id)
               :method "post"
               :accept-charset #+(or) "ISO-10646-UTF-1"
                               "UTF-8"
               :enctype #+(or) "multipart/form-data"
                        "application/x-www-form-urlencoded"
        (<:div :style "display: none"
         (<:input :type "hidden"
                  :name "id"
                  :value (prin1-to-string id))
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
          (<:as-is "Ver&ouml;ffentlichen"))))))))


(defun call-with-web-journal (page-title thunk &key canonical-uri)
  ;; TODO: Check how to make Squid not wait for the CGI script's
  ;;       termination, which makes generating a Last-Modified header
  ;;       feel slower to the end user rather than faster.
  ;;
  (when (eq *mode* :http)
    (http-add-header "Last-Modified" (http-timestamp (compute-journal-last-modified-date)))
    (http-add-header "Content-Language" "de")
    (http-add-header "Cache-Control" "public")
    (http-add-header "X-Pingback" (link-to :pingback :absolute t))
    (http-send-headers "text/html; charset=UTF-8"))

  (<xhtml :xmlns "http://www.w3.org/1999/xhtml"
          :lang "de"
   (<:head
    ;; This meta tag is, in fact, a lie, as we are actually
    ;; application/xhtml+xml, but without it, the encoding can not be
    ;; derived from the code alone by HTML parsers.  (It can be by XML
    ;; parses because of the XML preamble.)  This would make saving the
    ;; page on a disk inconvenient.
    (<:meta :http-equiv "Content-Type"
            :content "text/html; charset=UTF-8")
    ;; The iPhone's Mobile Safari browser scales all web pages by
    ;; default in order to make them look as if on a PC-sized monitor.
    ;; That's great for all those flashy web sites out there that aren't
    ;; designed to work with small devices.  It's not so great when your
    ;; pages mostly consist of lots of text to read, though.  Therefore,
    ;; let's disable the default scaling here.
    (<:meta :name "viewport" :content "initial-scale=1.0, width=device-width")
    (<:title
     (<:as-is
      (if page-title
          (format nil "~A &#8212; Kompottkins Weisheiten" page-title)
          "Kompottkins Weisheiten")))
    (<:link :rel "alternate"
            :type "application/atom+xml"
            :href (link-to :view-atom-feed)
            :title "Kompottkins weiser Atom-Feed")
    (<:link :rel "replies"
            :type "application/atom+xml"
            :href (link-to :view-comment-feed)
            :title "Kompottkins weiser Kommentarfeed")
    (<:link :rel "service.feed"
            :type "application/atom+xml"
            :href (link-to :view-atom-entry :absolute t)
            :title "Kompottkins Weisheiten")
    (<:as-is (format nil "<link rel=\"pingback\" href=\"~A\" />" (link-to :pingback :absolute t)))
    (<:link :rel "stylesheet" :type "text/css" :href (link-to :css))
    (<:link :rel "openid.server" :href "https://meinguter.name/index.php/serve")
    (<:link :rel "openid.delegate" :href "https://matthias.benkard.meinguter.name")
    (when canonical-uri
      (<:link :rel "canonical" :type "text/html" :href canonical-uri)))
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
    (when (and *journal-warnings* (eq *mode* :http))
      (<:div :id :warnings
       (dolist (warning *journal-warnings*)
         (<:div :class :journal-warning
          (<:p (<:strong "Achtung!"))
          (<:as-is warning)))))
    (<:div :id :contents
     (funcall thunk))
    (<:div :id :navigation))


    (when *debugging-p*
      (loop for (x . y) in `(("Action" . ,*action*)
                             ("Entry ID" . ,*post-number*)
                             ("Request method" . ,*method*)
                             ("Query" . ,*query*)
                             ("Query string" . ,(http-get-query-string))
                             ("Subpath" . ,*subpath*)
                             ("Environment" . ,(http-get-env-vars))
                             #+clisp ("Environment #2" . ,(ext:getenv)))
         do (<:p
             (<:hr)
             (<:h2 (<:as-html x))
             (<:p "Type " (<:em (<:as-html (type-of y))) ".")
             (<:pre (<:as-html (prin1-to-string y))))))))


(defun show-web-journal ()
  #.(locally-enable-sql-reader-syntax)
  (revalidate-cache-or-die "text/html; charset=UTF-8")
  (with-web-journal ((if (member *action* '(:view :edit :preview :post-comment
                                            :save-entry))
                         (title-of (find-entry *post-number*))
                         nil)
                     :canonical-uri (case *action*
                                      (:view
                                       (link-to :view :post-id *post-number* :absolute t))
                                      ((:index nil)
                                       (link-to :index :absolute t))))
    (case *action*
      ((:index nil)
       (let ((entries (select 'journal-entry
                              :order-by '(([date] :desc))
                              :flatp t))
             (full-journal-view (or (equal (getf *query* :|| nil) "full")
                                    (and (listp (getf *query* :|| nil))
                                         (member "full"
                                                 (getf *query* :|| nil)
                                                 :test #'equal)))))
         (dolist (entry (if full-journal-view
                            entries
                            (subseq entries 0 5)))
           (let ((*full-entry-view* t))
             (show-journal-entry entry)))
         (unless full-journal-view
           (<:div :class :old-entries
            (<:h2 (<:as-is "&Auml;ltere Eintr&auml;ge"))
            (<:p
             (<:a :href (link-to :full-index)
              (<:as-is "Alle Eintr&auml;ge vollst&auml;ndig anzeigen (langsam!).")))
            (<:table :class :old-entry-table
             (<:caption (<:as-is "Eintr&auml;ge nach Datum"))
             (<:thead
              (<:tr
               (<:th (<:as-is "Titel"))
               (<:th (<:as-is "Datum"))
               (<:th (<:as-is "Kommentare"))))
             (<:tbody
              (dolist (entry entries)
                (let ((*full-entry-view* nil))
                  (show-journal-entry entry)))))))))
      ((:view :post-comment :save-entry)
       (show-journal-entry (find-entry *post-number*) :comments-p t))))
  #.(restore-sql-reader-syntax-state))


(defun show-comment (comment)
  (with-slots (author body date id email website)
      comment
    (<:div :class :journal-comment
           :id (format nil "comment-~D" id)
     (<:div :class :journal-comment-header
      (<:as-html (format nil "(~A) "
                         (format-date nil "%day%.%mon%.%yr%, %hr%:%min%" date)))
      (<:a :href website :rel "nofollow"
       (<:as-html (format nil "~A" author)))
      (<:as-html " meint: "))
     (<:div :class :journal-comment-body
      (<:as-html (render-comment-body body))))))

(defun show-trackback (trackback)
  (with-slots (title excerpt date id url blog-name)
      trackback
    (<:div :class :journal-comment
           :id (format nil "trackback-~D" id)
     (<:div :class :journal-comment-header
            (<:as-html (format nil "(~A) "
                               (format-date nil "%day%.%mon%.%yr%, %hr%:%min%" date)))
            (<:strong (<:as-html (format nil "~A " (or blog-name url))))
            (if (null title)
                (<:a :href url :rel "nofollow" (<:as-html "schreibt hierzu:"))
                (progn
                  (<:as-html "schreibt hierzu im Artikel ")
                  (<:a :href url :rel "nofollow" (<:as-html (format nil "~A" title)))
                  (<:as-html ":"))))
     (<:div :class :journal-comment-body
      (<:as-html (render-comment-body excerpt))))))

(defun show-pingback (pingback)
  (with-slots (date id url)
      pingback
    (<:div :class :journal-comment
           :id (format nil "pingback-~D" id)
     (<:div :class :journal-comment-header
      (<:as-html (format nil "(~A) "
                         (format-date nil "%day%.%mon%.%yr%, %hr%:%min%" date)))
      (<:as-html "Pingback von ")
      (<:a :href url :rel "nofollow" (<:as-html url))
      (<:as-html ".")))))

(defun show-moderation-page ()
  #.(locally-enable-sql-reader-syntax)
  (revalidate-cache-or-die "text/html; charset=UTF-8")
  (with-web-journal (nil)
    (<:h2 (<:as-html "Trackbacks"))
    (dolist (trackback (select 'journal-trackback :flatp t :order-by '([date]) :where (clsql:sql-null [spam_p])))
      (<:hr)
      (<:form :action (link-to :moderation-page)
              :method "post"
              :accept-charset "UTF-8"
              :enctype "application/x-www-form-urlencoded"
              :style "display: inline"
        (<:input :type "hidden" :name "id" :value (prin1-to-string (id-of trackback)))
        (<:input :type "hidden" :name "type" :value "trackback")
        (<:input :type "hidden" :name "acceptp" :value "f")
        (<:button :type "submit" (<:as-is "Verwerfen")))
      (<:form :action (link-to :moderation-page)
              :method "post"
              :accept-charset "UTF-8"
              :enctype "application/x-www-form-urlencoded"
              :style "display: inline"
        (<:input :type "hidden" :name "id" :value (prin1-to-string (id-of trackback)))
        (<:input :type "hidden" :name "type" :value "trackback")
        (<:input :type "hidden" :name "acceptp" :value "t")
        (<:button :type "submit" (<:as-is "Annehmen")))
      (<:div (<:as-html "Zu: ") (<:a :href (link-to :view :post-id (id-of (entry-of trackback)) :absolute t) (<:as-html (title-of (entry-of trackback)))))
      (show-trackback trackback))
    (<:h2 (<:as-html "Pingbacks"))
    (dolist (pingback (select 'journal-pingback :flatp t :order-by '([date]) :where (clsql:sql-null [spam_p])))
      (<:hr)
      (<:form :action (link-to :moderation-page)
              :method "post"
              :accept-charset "UTF-8"
              :enctype "application/x-www-form-urlencoded"
              :style "display: inline"
        (<:input :type "hidden" :name "id" :value (prin1-to-string (id-of pingback)))
        (<:input :type "hidden" :name "type" :value "pingback")
        (<:input :type "hidden" :name "acceptp" :value "f")
        (<:button :type "submit" (<:as-is "Verwerfen")))
      (<:form :action (link-to :moderation-page)
              :method "post"
              :accept-charset "UTF-8"
              :enctype "application/x-www-form-urlencoded"
              :style "display: inline"
        (<:input :type "hidden" :name "id" :value (prin1-to-string (id-of pingback)))
        (<:input :type "hidden" :name "type" :value "pingback")
        (<:input :type "hidden" :name "acceptp" :value "t")
        (<:button :type "submit" (<:as-is "Annehmen")))
      (<:div (<:as-html "Zu: ") (<:a :href (link-to :view :post-id (id-of (entry-of pingback)) :absolute t) (<:as-html (title-of (entry-of pingback)))))
      (show-pingback pingback))
    (<:h2 (<:as-html "Kommentare"))
    (dolist (comment (select 'journal-comment :flatp t :order-by '([date]) :where (clsql:sql-null [spam_p])))
      (<:hr)
      (<:form :action (link-to :moderation-page)
              :method "post"
              :accept-charset "UTF-8"
              :enctype "application/x-www-form-urlencoded"
              :style "display: inline"
        (<:input :type "hidden" :name "id" :value (prin1-to-string (id-of comment)))
        (<:input :type "hidden" :name "type" :value "comment")
        (<:input :type "hidden" :name "acceptp" :value "f")
        (<:button :type "submit" (<:as-is "Verwerfen")))
      (<:form :action (link-to :moderation-page)
              :method "post"
              :accept-charset "UTF-8"
              :enctype "application/x-www-form-urlencoded"
              :style "display: inline"
        (<:input :type "hidden" :name "id" :value (prin1-to-string (id-of comment)))
        (<:input :type "hidden" :name "type" :value "comment")
        (<:input :type "hidden" :name "acceptp" :value "t")
        (<:button :type "submit" (<:as-is "Annehmen")))
      (<:div (<:as-html "Zu: ") (<:a :href (link-to :view :post-id (id-of (entry-of comment)) :absolute t) (<:as-html (title-of (entry-of comment)))))
      (show-comment comment)))
  #.(restore-sql-reader-syntax-state))

(defun preview-entry (title body id)
  (with-web-journal (title)
    (<:form :action (link-to :save :post-id id)
            :method "post"
            :accept-charset "UTF-8"
            :enctype "application/x-www-form-urlencoded"
      (when id
        (<:input :type "hidden"
                 :name "id"
                 :value (prin1-to-string id)))
      (<:input :type "hidden"
               :name "title"
               :value title)
      (<:input :type "hidden"
               :name "body"
               :value body)
      (<:div
       (<:button :type "submit"
        (<:as-is "Ver&ouml;ffentlichen"))))
    (show-journal-entry-with-components (or id -1)
                                        title
                                        body
                                        nil
                                        (get-universal-time)
                                        "markdown"
                                        nil
                                        nil
                                        nil)
    ;; Editor here.
    (<:form :action (link-to :preview :post-id id)
            :method "post"
            :accept-charset "UTF-8"
            :enctype "application/x-www-form-urlencoded"
      (<:div :style "display: none"
       (when id
         (<:input :type "hidden"
                  :name "id"
                  :value (prin1-to-string id))))
      (<:div :style "display: table"
       (<:div :style "display: table-row"
        (<:div :style "display: table-cell; vertical-align: top"
         (<:label :for "entry-title-editor"
                  :style "vertical-align: top"
          (<:as-is "&Uuml;berschrift: ")))
        (<:div :style "display: table-cell;"
         (<:input :type "text"
                  :name "title"
                  :value title
                  :id "entry-title-editor")))
       (<:div :style "display: table-row"
        (<:div :style "display: table-cell; vertical-align: top"
         (<:label :for "entry-body-editor"
                  :style "vertical-align: top"
          (<:as-html "Kommentar: ")))
        (<:div :style "display: table-cell"
         (<:textarea :name "body"
                     :id "entry-body-editor"
                     :rows 20
                     :cols 65
           (<:as-html body)))))
      (<:div
       (<:button :type "submit"
        (<:as-is "Vorschau"))))))


(defun show-debugging-page ()
  (http-add-header "Content-Language" "de")
  (http-send-headers "text/html; charset=UTF-8")

  (<xhtml :xmlns "http://www.w3.org/1999/xhtml"
          :lang "de"
    (when *debugging-p*
      (loop for (x . y) in `(("Action" . ,*action*)
                             ("Entry ID" . ,*post-number*)
                             ("Request method" . ,*method*)
                             ("Query" . ,*query*)
                             ("Query string" . ,(http-get-query-string))
                             ("Subpath" . ,*subpath*)
                             ("Environment" . ,(http-get-env-vars))
                             #+clisp ("Environment #2" . ,(ext:getenv)))
            do (<:p
                 (<:hr)
                 (<:h2 (<:as-html x))
                 (<:p "Type " (<:em (<:as-html (type-of y))) ".")
                 (<:pre (<:as-html (prin1-to-string y))))))))

(defun show-site-map ()
  #.(locally-enable-sql-reader-syntax)
  (with-xml-output (*standard-output* :encoding "utf-8")
    (with-tag ("urlset" '(("xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9")))
      (with-tag ("url")
        (emit-simple-tags :loc (link-to :index :absolute t)
                          :priority "0.5"))
      (with-tag ("url")
        (emit-simple-tags :loc (link-to :full-index :absolute t)
                          :priority "0.3"))
      (dolist (id (select [slot-value 'journal-entry 'id]
                          :from [journal-entry]
                          :flatp t))
        (with-tag ("url")
          (emit-simple-tags :loc (link-to :view :post-id id :absolute t)
                            :priority "0.7")))))
  #.(restore-sql-reader-syntax-state))

(defun update-journal ()
  (format t "~&Updating index page...")
  (update-index-page)
  (format t "~&Updating individual journal entries...")
  (update-all-journal-entry-pages)
  (format t "~&Updating the news feeds...")
  (update-atom-feed)
  (update-comment-feed)
  (format t "~&Updating the site map...")
  (update-site-map))

(defun update-index-page ()
  (let ((file-path (merge-pathnames "index.xhtml" *static-dir*)))
    (with-open-file (*standard-output* file-path :direction :output :if-exists :supersede)
      (with-yaclml-stream *standard-output*
        (let ((*mode* :file)
              (*action* :index))
          (show-web-journal))))))

(defun update-all-journal-entry-pages ()
  #.(locally-enable-sql-reader-syntax)
  (dolist (entry (select 'journal-entry :order-by '(([id] :asc)) :flatp t))
    (update-journal-entry-page entry))
  #.(restore-sql-reader-syntax-state))

(defun update-journal-entry-page (entry)
  (with-slots (id title) entry
     (let* ((file-name (format nil "~D.xhtml" id))
            (file-path (merge-pathnames file-name *static-dir*)))
       (with-open-file (*standard-output* file-path :direction :output :if-exists :supersede)
         (with-yaclml-stream *standard-output*
           (let ((*mode* :file))
             (with-web-journal (title :canonical-uri (link-to :view :post-id id :absolute t))
               (show-journal-entry entry :comments-p t))))))))

(defun update-atom-feed ()
  (let ((file-path (merge-pathnames "feed.xml" *static-dir*)))
    (with-open-file (*standard-output* file-path :direction :output :if-exists :supersede)
      (let ((*mode* :file))
        (show-atom-feed)))))

(defun update-comment-feed ()
  (let ((file-path (merge-pathnames "comment-feed.xml" *static-dir*)))
    (with-open-file (*standard-output* file-path :direction :output :if-exists :supersede)
      (let ((*mode* :file))
        (show-comment-feed)))))

(defun update-site-map ()
  (let ((file-path (merge-pathnames "sitemap.xml" *static-dir*)))
    (with-open-file (*standard-output* file-path :direction :output :if-exists :supersede)
      (let ((*mode* :file))
        (show-site-map)))))
