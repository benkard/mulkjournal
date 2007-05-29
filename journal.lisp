#! /usr/bin/env clisp
;;;; -*- coding: utf-8; mode: lisp -*-
;;;; Copyright 2007, Matthias Andreas Benkard.


;;; TAKE NOTICE: If you want to run this script from the command line or
;;; from a web server, be sure to use a core image including the systems
;;; this script depends upon.  The DEFSYSTEM form below has mainly been
;;; written for purposes of documentation.
(asdf:defsystem #:mulk.journal
  :description "Matthias Benkard's simple web journal engine"
  :licence     "Affero General Public License, version 1 or higher"
  :depends-on  (#:cl-ppcre #:cl-fad #:iterate #:cl-markdown #:parenscript
                #:yaclml #:lisp-cgi-utils #:alexandria))


;;; The following does not generally work in a CGI setting because of
;;; security restrictions.  Loading all the dependencies individually
;;; rather than using a core image would certainly be too slow for any
;;; serious CGI usage, anyway, so what the heck.
#+nil (asdf:oos 'asdf:load-op '#:mulk.journal)


(defpackage #:mulk.journal
  (:nicknames #:journal)
  (:use #:cl #:fad #:iterate #:markdown #:yaclml #:http #:alexandria))

(in-package #:mulk.journal)


(defun keywordify (thing)
  (if (null thing)
      thing
      (intern (etypecase thing
                (string (string-upcase thing))
                (symbol (symbol-name   thing)))
              '#:keyword)))


(defparameter *query*
  (mapcan #'(lambda (param)
              (list (keywordify param)
                    (http-query-parameter param)))
          (http-query-parameter-list))
  "The HTTP query string transformed into a property list.")

(defparameter *action*
  (keywordify (getf *query* :action))
  "One of NIL, :INDEX, :VIEW-ATOM-FEED, :VIEW, :POST, :EDIT, and :PREVIEW.")

(defparameter *post-number*
  (parse-integer (getf *query* :post "")
                 :junk-allowed t  #|| :radix 12 ||#)
  "The identification number of the journal entry to be acted upon.
   May be NIL.")

(defparameter *method*
  (keywordify (gethash "REQUEST_METHOD" (http-get-env-vars)))
  "One of :GET, :POST, :PUT, and :DELETE.")

(defparameter *journal-entries*
  '()
  "A list of JOURNAL-ENTRY objects.")

(defparameter *http-env*
  (http-get-env-vars)
  "A hash table of HTTP environment variables.")


(defclass journal-entry ()
  ((id :type (integer 0)
       :accessor id-of
       :initarg :id)
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
          (setf markup #+nil
                (delete-if (constantly t)
                           markup
                           :start (1- (second matches))
                           :end (second matches))
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
        (apply #'make-instance 'journal-entry data)))))


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


(defun format-date (destination date-control-string universal-time)
  "Format DATE according to the description given by DATE-FORMAT-STRING.

Recognised format directives are: %day, %mon, %yr, %day-of-week, %zone,
%@day-of-week (name of day), %sec, %min, %hr, %daylight-p.

Note that you can mix FORMAT and FORMAT-DATE painlessly by calling them
after another in any arbitrary order."
  (format
   destination "~A"
   (with-output-to-string (out)
     (multiple-value-bind (sec min hr day mon yr day-of-week daylight-p zone)
         (decode-universal-time universal-time)
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
                   ("^%hr"           (values "~D" hr   3)))
               (when first-match-p
                 (format out (subseq date-control-string 0 start))
                 (setf first-match-p nil))
               (if control
                   (progn
                     (format out control value)
                     (format out "~A" (subseq substring offset)))
                   (format out "~A" substring))))))))))


(defun show-atom-feed ()
  (http-add-header "Content-type" "text/xml; charset=UTF-8")
  (http-send-headers))


(defun show-journal-entry (journal-entry)
  (<:div :class :journal-entry
   (<:h2 (<:a :href (format nil
                            "journal.cgi?action=view&post=~D"
                            (id-of journal-entry))
              (<:as-html (title-of journal-entry))))
    (<:div :class :journal-entry-header
     (<:span :class :journal-entry-date
      (<:as-html
       (format-date nil "%@day-of-week, den %day.%mon.%yr, %hr:%min."
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
             :method "DELETE"
             :action "journal.cgi"
      (<:input :type "hidden"
               :name "action"
               :value "delete")
      (<:input :type "hidden"
               :name "post"
               :value (prin1-to-string (id-of journal-entry)))
      (<:button :type "submit"
                ;;:style "display: inline;"
                (<:as-is "L&ouml;schen")))
     " | "
     (<:form :class :journal-entry-delete-button-form
             :style "display: inline;"
             :method "GET"
             :action "journal.cgi"
      (<:input :type "hidden"
               :name "action"
               :value "edit")
      (<:input :type "hidden"
               :name "post"
               :value (prin1-to-string (id-of journal-entry)))
      (<:button :type "submit"
                ;;:style "display: inline;"
                (<:as-is "Bearbeiten")))
     #+nil
     (<:a :href (format nil
                        "journal.cgi?action=edit&post=~D"
                        (id-of journal-entry))
          (<:as-is "Bearbeiten"))
     " | "
     (<:a :href (format nil
                        "journal.cgi?action=view&post=~D"
                        (id-of journal-entry))
          (<:as-is
           (format nil "~D Kommentare" (length (comments-about journal-entry))))))))


(defun show-web-journal ()
  (http-add-header "Content-type" "text/html; charset=UTF-8")
  (http-send-headers)

  (<:html
   (<:head
    (<:title
     (<:as-html
      (if (member *action* '(:view :edit :preview))
          (format nil "~A -- Kompottkins Weisheiten"
                  (title-of (find-entry *post-number*)))
          "Kompottkins Weisheiten")))
    (<:link :rel "stylesheet" :type "text/css" :href "../journal.css"))
   (<:body
    (<:div :id :main-title-box
     (<:h1 :id :main-title
           (<:a :href "journal.cgi?action=index"
                "Kompottkins Weisheiten"))
     (<:div :id :main-subtitle (<:as-is
                                "NEU! Jetzt ohne regelm&auml;&szlig;ige
                                 Serverabst&uuml;rze!")))
    (<:div :id :contents
     (case *action*
       ((:index nil)
        (mapc #'show-journal-entry *journal-entries*))
       ((:view)
        (show-journal-entry (find-entry *post-number*))))))
    (<:div :id :navigation)

    #+debug
    (loop for (x . y) in `(("Action" . ,*action*)
                           ("Request method" . ,*method*)
                           ("Query" . ,*query*)
                           ("Query string" . ,(http-get-query-string))
                           ("Environment" . ,(http-get-env-vars)))
       do (<:p
           (<:hr)
           (<:h2 (<:as-html x))
           (<:p "Type " (<:em (<:as-html (type-of y))) ".")
           (<:pre (<:as-html (prin1-to-string y)))))))


(defun main ()
  (let ((*journal-entries* (read-journal-entries)))
    (case *action*
      (:view-atom-feed (show-atom-feed))
      (otherwise       (show-web-journal)))))


(handler-bind
    ((error #'
      (lambda (e)
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
