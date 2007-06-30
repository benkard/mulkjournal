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


(defun keywordify (thing)
  (if (null thing)
      thing
      (intern (etypecase thing
                (string (string-upcase thing))
                (symbol (symbol-name   thing)))
              '#:keyword)))


(defun make-uuid ()
  "Generate a version 4 UUID according to RFC 4122, section 4.4."
  (format nil "~(~8,'0x-~4,'0x-~4,'0x-~2,'0x~2,'0x-~12,'0x~)"
          (random #x100000000)                ;; time_low
          (random #x10000)                    ;; time_mid
          (logior #b0100000000000000
                  (logand #b0000111111111111
                          (random #x10000))) ;; time_hi_and_version
          (logior #b10000000
                  (logand #b00111111
                          (random #x100))) ;; clock_seq_hi_and_reserved
          (random #x100)                   ;; clock_seq_low
          (random #x1000000000000)))       ;; node


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


(defun name-of-day (day-of-week)
  (case day-of-week
    (0 "Montag")
    (1 "Dienstag")
    (2 "Mittwoch")
    (3 "Donnerstag")
    (4 "Freitag")
    (5 "Samstag")
    (6 "Sonntag")))


(defun %real-format-date (destination date-control-string universal-time
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


(defun compute-script-last-modified-date ()
  #-clisp (get-universal-time)
  #+clisp
  (loop for file in (list* *script-filename*                    ;; journal.cgi
                           (remove-if-not #'(lambda (p)
                                              (equal "lisp"
                                                     (pathname-type p)))
                                          (list-directory *script-dir*)))
        maximize (posix:file-stat-mtime (posix:file-stat file))))


(defun read-to-array (stream &key (initial-length 128))
  "Read elements from a stream repeatedly until the end of file is
reached and write the results into a newly created array of the same
ELEMENT-TYPE as the stream's."
  (do* ((buffer          (make-array (list initial-length)
                                     :element-type (stream-element-type stream)
                                     :adjustable t
                                     :fill-pointer t))
        (last-unmodified (read-sequence buffer stream)
                         (read-sequence buffer stream :start old-length))
        (read-elements   last-unmodified
                         (- last-unmodified old-length))
        (old-length      initial-length length)
        (length          (ceiling (* 1.5 old-length))
                         (ceiling (* 1.5 old-length))))
       ((< last-unmodified old-length)
        (setf (fill-pointer buffer) last-unmodified)
        buffer)
    (setf buffer (adjust-array buffer (list length)
                               :fill-pointer length))))


(defun call-with-result-cache (cache-id fun &key (younger-than nil younger-p))
  (let ((cache-file (merge-pathnames (make-pathname :name (format nil
                                                                  "CACHE-~A"
                                                                  cache-id))
                                     *cache-dir*)))
    (if (and (directory-exists-p *cache-dir*)
             (file-exists-p cache-file)
             #-clisp nil
             #+clisp (or (not younger-p)
                         (> (posix:file-stat-mtime (posix:file-stat cache-file))
                            younger-than)))
        (with-open-file (in cache-file
                            :direction :input
                            :external-format #+clisp charset:utf-8
                                             #+sbcl :utf-8)
          (read-to-array in))
        (with-open-file (out (if (directory-exists-p *cache-dir*)
                                 cache-file
                                 #p"/dev/null")
                             :direction :output
                             :if-exists :supersede
                             :external-format #+clisp charset:utf-8
                                              #+sbcl :utf-8)
          (let ((string (funcall fun)))
            (princ string out)
            string)))))


(defun format-date (destination date-control-string universal-time
                    &optional (time-zone nil time-zone-supplied-p))
  (with-result-cache ((format nil "date-format-~D-~A-~A"
                              universal-time date-control-string time-zone)
                      :younger-than (compute-script-last-modified-date))
    (apply #'%real-format-date
           destination date-control-string universal-time
           (and time-zone-supplied-p time-zone))))
