#! /usr/bin/env clisp
;;;; -*- coding: utf-8; mode: lisp -*-
;;;; Copyright 2007, Matthias Andreas Benkard.

;;;-----------------------------------------------------------------------
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
;;;-----------------------------------------------------------------------


;;; TAKE NOTICE: If you want to run this script from the command line or
;;; from a web server, be sure to use a core image including the systems
;;; this script depends upon.  The DEFSYSTEM form below has mainly been
;;; written for purposes of documentation.
(asdf:defsystem #:mulk.journal
  :description "Matthias Benkard's simple web journal engine"
  :licence     "Affero General Public License, version 1 or higher"
  :depends-on  (#:cl-ppcre #:cl-fad #:iterate #:cl-markdown #:parenscript
                #:yaclml #:lisp-cgi-utils #:alexandria #:xml-emitter
                #:split-sequence))


;;; The following does not generally work in a CGI setting because of
;;; security restrictions.  Loading all the dependencies individually
;;; rather than using a core image would certainly be too slow for any
;;; serious CGI usage, anyway, so what the heck.
(unless (find-package '#:http)
  (asdf:oos 'asdf:load-op '#:mulk.journal))


(defpackage #:mulk.journal
  (:nicknames #:journal)
  (:use #:cl #:fad #:iterate #:markdown #:yaclml #:http #:alexandria
        #:xml-emitter #:split-sequence))

(in-package #:mulk.journal)


(defun keywordify (thing)
  (if (null thing)
      thing
      (intern (etypecase thing
                (string (string-upcase thing))
                (symbol (symbol-name   thing)))
              '#:keyword)))


(defparameter *site*
  (if (file-exists-p #p"/home/mulk") :mst-plus :nfs.net)
  "One of :WIRSELKRAUT and :NFS.NET.")

(defparameter *debugging-p*
  (eq *site* :mst-plus))

(defparameter *query*
  #+clisp
  (mapcan #'(lambda (param)
              (list (keywordify param)
                    (ext:convert-string-from-bytes
                     (ext:convert-string-to-bytes
                      (http-query-parameter param)
                      charset:iso-8859-1)
                     charset:utf-8)))
          (http-query-parameter-list))
  #-clisp '()
  "The HTTP query string transformed into a property list.")

(defparameter *http-env*
  (http-get-env-vars)
  "A hash table of HTTP environment variables.")

(defparameter *subpath-query*
  (subseq (gethash "REQUEST_URI" *http-env*)
          (length (if (eq *site* :mst-plus)
                      (gethash "SCRIPT_NAME" *http-env*)
                      "/journal")))
  "The query string stripped of the script location.")

(defparameter *subpath-string*
  (subseq *subpath-query*
          0
          (or (position #\? *subpath-query*)
              (length *subpath-query*)))
  "The query string stripped of the script location and query parameters,
   that is, the virtual path below the script.")

(defparameter *subpath*
  (split-sequence #\/ *subpath-string*
                  :remove-empty-subseqs t)
  "*SUBPATH-STRING* parsed into a list of nested directories.")

(defparameter *post-number*
  (parse-integer (or (first *subpath*)
                     (getf *query* :id ""))
                 :junk-allowed t  #|| :radix 12 ||#)
  "The identification number of the journal entry to be acted upon.
   May be NIL.")

(defparameter *action*
  (or (keywordify (getf *query* :action))
      (cond (*post-number*                      :view)
            ((string= "feed" (first *subpath*)) :view-atom-feed)
            (t                                  nil)))
  "One of NIL, :INDEX, :VIEW-ATOM-FEED, :VIEW, :POST, :EDIT, :PREVIEW,
   and :POST-COMMENT.")

(defparameter *method*
  (keywordify (gethash "REQUEST_METHOD" (http-get-env-vars)))
  "One of :GET, :POST, :PUT, and :DELETE.")

(defparameter *journal-entries*
  '()
  "A list of JOURNAL-ENTRY objects.")


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


(defun make-uuid ()
  "Generate a version 4 UUID according to RFC 4122, section 4.4."
  (format nil "~(~8,'0x-~4,'0x-~4,'0x-~2,'0x~2,'0x-~12,'0x~)"
          (random #x100000000)                ;; time_low
          (random #x10000)                    ;; time_mid
          (logior #b0100000000000000
                  (logand #b0000111111111111
                          (random #x10000)))  ;; time_hi_and_version
          (logior #b10000000
                  (logand #b00111111
                          (random #x100)))    ;; clock_seq_hi_and_reserved
          (random #x100)                      ;; clock_seq_low
          (random #x1000000000000)))          ;; node


(defun fixup-markdown-output (markup)
  ;; No, cl-markdown is certainly not perfect.
  ;;
  ;; First, convert "<a ...> bla</a>" into " <a ...>bla</a>" (note the
  ;; excess space to the right of the opening tag in the unprocessed
  ;; string, which we move to the left of the same opening tag, where we
  ;; expect it to make more sense in the general case).
  (loop
     for matches = (ppcre:all-matches "<a [^>]*?> " markup)
     while (not (null matches))
     do (progn
          (setf markup
                (replace markup markup :start1 (1+ (first matches))
                                       :end1 (second matches)
                                       :start2 (first matches)
                                       :end2 (1- (second matches))))
          (setf (elt markup (first matches)) #\Space)))
  markup)


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


(defun read-journal-entries ()
  (let ((directory
         (make-pathname
          :directory (pathname-directory
                      (merge-pathnames
                       (make-pathname :directory '(:relative "journal-entries")
                                      :name nil)
                       (pathname-as-file
                        (or (gethash "SCRIPT_FILENAME" *http-env*)
                            "/home/mulk/Dokumente/Projekte/Mulkblog/journal.cgi"))))))
        (journal-entries (list)))
    (when (file-exists-p directory)
      (walk-directory directory
                      #'(lambda (x)
                          (push (read-journal-entry x) journal-entries))
                      :test (complement #'directory-pathname-p)))
    (sort journal-entries #'>= :key #'id-of)))


(defmacro regex-case (string &body clauses)
  (once-only (string)
    `(cond ,@(loop for (keys . forms) in clauses
                collect
                  `(,(if (and (symbolp keys)
                              (or (eq t keys)
                                  (equal "OTHERWISE" (symbol-name keys))))
                         't
                         `(or ,@(loop for key in (if (listp keys)
                                                     keys
                                                     (list keys))
                                   collect
                                     `(ppcre:scan-to-strings ,key ,string))))
                     ,@forms)))))


(defun name-of-day (day-of-week)
  (case day-of-week
    (0 "Montag")
    (1 "Dienstag")
    (2 "Mittwoch")
    (3 "Donnerstag")
    (4 "Freitag")
    (5 "Samstag")
    (6 "Sonntag")))


(defun format-date (destination date-control-string universal-time
                    &optional (time-zone nil time-zone-supplied-p))
  "Format DATE according to the description given by DATE-FORMAT-STRING.

Recognised format directives are: %day, %mon, %yr, %day-of-week, %zone,
%@day-of-week (name of day), %sec, %min, %hr, %daylight-p.

Note that you can mix FORMAT and FORMAT-DATE painlessly by calling them
after another in any arbitrary order."
  (format
   destination "~A"
   (with-output-to-string (out)
     (multiple-value-bind (sec min hr day mon yr day-of-week daylight-p zone)
         (if time-zone-supplied-p
             (decode-universal-time universal-time time-zone)
             (decode-universal-time universal-time))
       (let ((first-match-p t))
         (ppcre:do-matches (start end "%[^%]*" date-control-string)
           (let ((substring (subseq date-control-string start end)))
             (multiple-value-bind (control value offset)
                 (regex-case substring
                   ("^%day-of-week"  (values "~D" day-of-week 12))
                   ("^%@day-of-week" (values "~A"
                                             (name-of-day day-of-week)
                                             13))
                   ("^%daylight-p"   (values "~A" daylight-p  11))
                   ("^%zone"         (values "~D" zone 5))
                   ("^%day"          (values "~D" day  4))
                   ("^%mon"          (values "~D" mon  4))
                   ("^%yr"           (values "~D" yr   3))
                   ("^%sec"          (values "~D" sec  4))
                   ("^%min"          (values "~D" min  4))
                   ("^%hr"           (values "~D" hr   3))
                   ("^%2day"         (values "~2,'0D" day  5))
                   ("^%2mon"         (values "~2,'0D" mon  5))
                   ("^%4yr"          (values "~4,'0D" yr   4))
                   ("^%2sec"         (values "~2,'0D" sec  5))
                   ("^%2min"         (values "~2,'0D" min  5))
                   ("^%2hr"          (values "~2,'0D" hr   4)))
               (when first-match-p
                 (format out (subseq date-control-string 0 start))
                 (setf first-match-p nil))
               (if control
                   (progn
                     (format out control value)
                     (format out "~A" (subseq substring offset)))
                   (format out "~A" substring))))))))))


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


(yaclml:deftag <xhtml (&attribute dir lang xmlns (prologue t) &body body)
  (when prologue
    (emit-princ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
  (emit-open-tag "html" `(("dir" . ,dir) ("lang" . ,lang) ("xmlns" . ,xmlns)))
  (emit-body body)
  (emit-close-tag "html"))


(defun show-web-journal ()
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
    (<:link :rel "stylesheet" :type "text/css" :href (link-to :css)))
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


#+clisp
(defun main ()
  (let ((*journal-entries* (read-journal-entries))
        (*random-state* (make-random-state t)))
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
        (otherwise       (show-web-journal))))))


#+clisp
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
  (main))
