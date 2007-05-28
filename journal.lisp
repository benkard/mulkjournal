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
                #:yaclml #:lisp-cgi-utils))


;;; The following does not generally work in a CGI setting because of
;;; security restrictions.  Loading all the dependencies individually
;;; rather than using a core image would certainly be too slow for any
;;; serious CGI usage, anyway, so what the heck.
#+nil (asdf:oos 'asdf:load-op '#:mulk.journal)


(defpackage #:mulk.journal
  (:nicknames #:journal)
  (:use #:cl #:fad #:iterate #:markdown #:yaclml #:http))

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
   (body :type string
         :accessor body-of
         :initarg :body
         :initform "")
   (categories :type list
               :accessor categories-of
               :initarg :categories
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


(defun show-atom-feed ()
  (http-add-header "Content-type" "text/xml; charset=UTF-8")
  (http-send-headers))


(defun show-web-journal ()
  (http-add-header "Content-type" "text/html; charset=UTF-8")
  (http-send-headers)

  (<:html
   (<:body
    (<:h1 :id :main-title "Kompottkins Weisheiten")
    (<:div :id :contents
     (if (or (null *action*)
             (eq *action* :index))
         (dolist (journal-entry *journal-entries*)
           (<:div :class :journal-entry
            (<:h2 (<:as-html (title-of journal-entry)))
            (<:as-is (journal-markup->html (body-of journal-entry)))))))
    (<:div :id :navigation)

    (loop for (x . y) in `(("Action" . ,*action*)
                           ("Request method" . ,*method*)
                           ("Query" . ,*query*)
                           ("Query string" . ,(http-get-query-string))
                           ("Environment" . ,(http-get-env-vars)))
       do (<:p
           (<:hr)
           (<:h2 (<:as-html x))
           (<:p "Type " (<:em (<:as-html (type-of y))) ".")
           (<:pre (<:as-html (prin1-to-string y))))))))


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
