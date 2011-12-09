;;;; scrapers.lisp

(in-package #:l1sp-org)

(defparameter *clisp-package-pattern*
  (create-scanner "<link linkend=\"(.*?)\">.*role=\"package\">(.*?)</quote>"))

(defparameter *clisp-var-pattern*
  (create-scanner "<link linkend=\"(.*?)\"><varname>(.*?)</varname>"))

(defparameter *clisp-fun-pattern*
  (create-scanner "<link linkend=\"(.*?)\"><function>(.*?)</function>"))

(defparameter *clisp-olink-pattern*
  (create-scanner "<olink.* targetptr=\"(.*?)\"><function>(.*?)</function>"))

(defparameter *clisp-patterns*
  (list *clisp-package-pattern*
        *clisp-olink-pattern*
        *clisp-var-pattern*
        *clisp-fun-pattern*))

(defmacro with-inout-streams ((instream infile &rest inargs)
                              (outstream outfile &rest outargs)
                              &body body)
  `(with-open-file (,instream ,infile ,@inargs)
     (with-open-file (,outstream ,outfile :direction :output
                                 :if-exists :supersede
                                 ,@outargs)
       ,@body)))

(defmacro for-each-line ((var stream) &body body)
  (let ((gstream (gensym)))
    `(let ((,gstream ,stream))
       (loop
          (let ((,var (read-line ,gstream nil)))
            (unless ,var (return))
            ,@body)))))

(defun spacy (string)
  (position #\Space string))

(defun convert-clisp-entities (in out)
  (let ((base "http://clisp.cons.org/impnotes.html"))
    (with-open-file (instream in)
      (with-open-file (outstream out :direction :output :if-exists :supersede)
        (format outstream "clisp ~A~%" base)
        (loop for line = (read-line instream nil)
           while line do
             (dolist (pattern *clisp-patterns*)
               (register-groups-bind (anchor symbol)
                   (pattern line)
                 (when pattern
                   (format outstream "clisp ~(~A t ~A#~A~)~%"
                           symbol base anchor)))))))))

;;; Converting the clim file from cl-irc

(defun convert-mcclim-table (in out)
  (let ((base "http://bauhh.dyndns.org/clim-spec/"))
    (with-open-file (instream in :external-format :latin1)
      (with-open-file (outstream out :direction :output :if-exists :supersede)
        (format outstream "clim ~A~%" base)
        (loop for data = (read instream nil)
           while data do
             (destructuring-bind (bogus-name (name noise (kw type)) suffix)
                 data
               (declare (ignore bogus-name noise kw))
               (unless (position #\space name)
                 (format outstream "clim ~A ~S ~A~A~%"
                         name type base suffix))))))))

;;; Converting the CCL index

(defparameter *ccl-index-entry-pattern*
  (create-scanner "<dt>(.+?), .*href=\"(.*?)\">(.*) "))

(defun convert-clozurecl-index (in out)
  (let ((base "http://ccl.clozure.com/manual/"))
    (with-inout-streams (instream in) (outstream out)
      (format outstream "ccl ~A~%" base)
      (loop for line = (read-line instream nil)
         while line do
           (register-groups-bind (sym suffix type)
               (*ccl-index-entry-pattern* line)
             (when (and sym (not (position #\Space sym)))
               (format outstream "ccl ~A ~S ~A~A~%"
                       sym type base suffix)))))))
      
;;; Converting the SBCL indexes

(defparameter *sbcl-index-entry-pattern*
  (create-scanner "<li><a href=\"(.*?)\"><code>(.*?)</code>.*>(.*)</a></li>"))

(defun convert-sbcl-index (in out)
  (let ((base "http://www.sbcl.org/manual/"))
    (with-inout-streams (instream in) (outstream out)
      (format outstream "sbcl ~A~%" base)
      (for-each-line (line instream)
        (register-groups-bind (suffix sym type)
            (*sbcl-index-entry-pattern* line)
          (when (and sym (not (position #\Space sym)))
            (format outstream "sbcl ~A ~S ~A~A~%"
                    sym type base suffix)))))))


;;; CFFI

(defparameter *cffi-index-entry-pattern*
  (create-scanner "<li><a href=\"(.*?)\"><code>(.*?)</code>"))

(defun convert-cffi-index (in out)
  (let ((base "http://common-lisp.net/project/cffi/manual/html_node/"))
    (with-inout-streams (instream in) (outstream out)
      (format outstream "cffi ~A~%" base)
      (for-each-line (line instream)
        (register-groups-bind (suffix sym)
            (*cffi-index-entry-pattern* line)
          (when (and sym (not (spacy sym)))
            (format outstream "cffi ~A t ~A~A~%"
                    sym base suffix)))))))

;;; mop dictionary

(defparameter *mop-index-entry-pattern*
  (create-scanner "<a name=\"(.*?)\"><i>(.*?)</i>"))

(defun convert-mop-index (in out)
  (let ((base "http://www.lisp.org/mop/index.html")
        (dict "http://www.lisp.org/mop/dictionary.html"))
    (with-inout-streams (instream in) (outstream out)
      (format outstream "mop ~A~%" base)
      (for-each-line (line instream)
        (register-groups-bind (sym type)
            (*mop-index-entry-pattern* line)
          (when (and sym (not (spacy sym)))
            (format outstream "mop ~A ~S ~A#~A~%"
                    sym type dict sym)))))))



;;; Big dumper

(defun table-keys (table)
  (loop for k being each hash-key of table collect k))

(defun dump-as-html (out)
  (with-open-file (stream out :direction :output :if-exists :supersede)
    (cl-who:with-html-output (s stream)
      (:html
       (:head
        (:title "L1sp.org active redirects")
        (:body
         (:h1 "L1sp.org active redirects")
         (:hr)
         (:ul
          (let ((keys (sort (table-keys *redirect-table*) #'string<)))
            (dolist (key keys)
              (htm
               (:li (:a :href (gethash key *redirect-table*) (esc key)))))))))))))

