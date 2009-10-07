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
  (reduce #'(lambda (string thing)
              (destructuring-bind (regex . replacement)
                  thing
                (ppcre:regex-replace-all regex
                                         string
                                         replacement)))
          (load-time-value
           (mapcar #'(lambda (thing)
                       (destructuring-bind (regex . replacement)
                           thing
                         (cons (ppcre:create-scanner regex) replacement)))
                   '(;; "<em>...</em> ." -> "<em>...</em>."
                     ("(</[^>]*?>) \\." . "\\1.")
                     ;; "<a ...> bla</a>" -> " <a ...>bla</a>"
                     ("(<a [^>]*?>) "   . " \\1"))))
          :initial-value markup))


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
           (and time-zone-supplied-p (list time-zone)))))


(defun single-object (list &optional (errorp t))
  (assert (null (cdr list)))
  (when errorp
    (assert (not (null list))))
  (first list))


(defun akismet-login ()
  (drakma:http-request "http://rest.akismet.com/1.1/verify-key"
                       :protocol :http/1.0
                       :method :post
                       :user-agent "Mulk Journal/0.0.1"
                       :parameters `(("key" . ,*wordpress-key*)
                                     ("blog" . "http://matthias.benkard.de/journal"))))


(defun akismet-check-comment (comment referrer)
  #.(locally-enable-sql-reader-syntax)
  (prog1
      (with-slots (submitter-user-agent submitter-ip body author website entry-id)
                  comment
         (drakma:http-request (format nil "http://~A.rest.akismet.com/1.1/comment-check" *wordpress-key*)
                              :protocol :http/1.0
                              :method :post
                              :user-agent "Mulk Journal/0.0.1"
                              :parameters `(("blog" . "http://matthias.benkard.de/journal")
                                            ("user_ip" . ,submitter-ip)
                                            ("user_agent" . ,submitter-user-agent)
                                            ,@(when referrer
                                                `(("referrer" . ,referrer)))
                                            ("permalink" . ,(link-to :view
                                                                     :post-id (first
                                                                               (select [id]
                                                                                       :from [journal-entry]
                                                                                       :where [= [id] entry-id]
                                                                                       :flatp t))))
                                            ("comment_type" . "comment")
                                            ("comment_author" . ,author)
                                            ("comment_author_url" . ,website)
                                            ("comment_content" . ,body))))
    #.(restore-sql-reader-syntax-state)))


(defun detect-spam (comment &key referrer)
  (ignore-errors
    (akismet-login)
    (string= "true" (akismet-check-comment comment referrer))))


(defun mail (address subject body)
  #-clisp
  (cerror "Can't send e-mail on this Lisp implementation.")
  #+clisp
  (let ((sendmail-stdin (ext:run-program "sendmail"
                                         :arguments (list address)
                                         :wait t
                                         :output nil
                                         :input :stream)))
    (format sendmail-stdin "~&To: ~A~
                            ~&MIME-Version: 1.0~
                            ~&Content-type: text/plain; charset=utf-8~
                            ~&Content-transfer-encoding: quoted-printable~
                            ~&Subject: =?utf-8?Q?~A?=~
                            ~&~%~
                            ~&~A"
            address
            (quote-printable subject nil)
            (quote-printable body t))
    (close sendmail-stdin)))


(defun char-octets (string-designator)
  #-clisp
  (error "Can't convert strings to byte vectors on this Lisp implementation.")
  #+clisp
  (ext:convert-string-to-bytes (string string-designator)
                               custom:*default-file-encoding*))


(let ((printable-chars
       ;; This list is incomplete, which shouldn't hurt.
       ;;
       ;; Note that the list is designed to be compatible with both the
       ;; Quoted-Printable and Q encodings.  Even though
       ;; Quoted-Printable itself specifies #\_, #\?, #\= and #\Space as
       ;; representing themselves, they may not be left unencoded here
       ;; because of this.  On the other hand, #\_ cannot be assumed to
       ;; encode #\Space, either, because this is only specified by Q,
       ;; not by Quoted-Printable.
       (cons #\Newline
             (coerce "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ,.-!~"
                     'list))))
  (defun quote-printable (text line-breaks-p)
    (with-output-to-string (out)
      (let ((i 0))
        (loop for char across text
              do (if (member char printable-chars)
                     (princ char out)
                     (loop for byte across (char-octets char)
                           do (format out "=~2,'0X" byte)))
              when (and line-breaks-p (= i 73))
                do (progn
                     (princ #\= out)
                     (terpri out)
                     (setq i 0))
              else
                do (incf i))))))


(defun mail-comment (address comment entry)
  (mail address
        (format nil "[Kommentar] ~A" (title-of entry))
        (format nil "~&Kommentar von: ~A~
                     ~&E-Mail: ~A~
                     ~&Website: ~A~
                     ~&IP-Adresse: ~A~
                     ~&Webbrowser: ~A~
                     ~&Als Spam erkannt: ~A~
                     ~&~%~
                     ~&~A"
                (author-of comment)
                (email-of comment)
                (website-of comment)
                (submitter-ip comment)
                (submitter-user-agent comment)
                (spamp comment)
                (body-of comment))))

(defun revalidate-cache-or-die ()
  #+clisp
    (let ((time (cybertiggyr-time:parse-time *if-modified-since*)))
      (when (and (integerp time) (>= *if-modified-since* (compute-journal-last-modified-date)))
        (http-add-header "Status: 304 Not Modified")
        (http-send-headers)
        (ext:quit 0)))
  #-clisp
    nil)
