;;;; redirector.lisp

(in-package #:l1sp-org)

;;; Redirecting

(defparameter *redirect-table* (make-hash-table :test 'equal))

(defun not-found ()
  (setf (tbnl:return-code*) tbnl:+http-not-found+)
  (throw 'done "Not found."))

(defun multiple-choice (prefix term alist)
  (let ((escaped (html-template:escape-string-all term)))
    (with-html-output-to-string (stream)
      (:html
       (:head
        (:title (fmt "~A - ~A - Disambiguation" prefix escaped))
        (:h1 (fmt "~A - ~A" prefix escaped))
        (:p (fmt "Please select which reference to <b>~A</b> you intended: "
                 escaped))
        (:ul
         (loop for ((key . value)) on alist do
              (htm
               (:li (:a :href value
                        (esc key)))))))))))

(defun parse-script-name (name)
  "Return the prefix and term for the script name NAME."
  (let ((start 1)
        (end (position #\/ name :start 1)))
    (when end
      (values (subseq name start end)
              (subseq name (1+ end))))))

(defun compound-prefix-match (prefix target &key (start 0))
  (loop for ch across prefix
     with tpos = start
     always (and (< tpos (length target))
                 (if (char-equal ch #\-)
                     (setf tpos (position #\- target :start tpos))
                     (char-equal ch (aref target tpos))))
     do (incf tpos)))


(defun urlsearch-matches (term path)
  (let* ((slash (position #\/ path :start 1))
         (colon (position #\: path :start (or slash 0)))
         (start (or colon slash)))
    (when start
      (or (search term path :start2 slash :test 'equalp)
          (compound-prefix-match term path :start (1+ start))))))

(defvar *search-result-limit* 250)

(defun urlsearch (term)
  (when (plusp (length term))
    (let ((results '())
          (count 0))
      (block nil
        (maphash (lambda (k v)
                   (when (urlsearch-matches term k)
                     (when (= (incf count) *search-result-limit*)
                       (return))
                     (push (cons k v)
                           results)))
                 *redirect-table*))
      (sort results #'string< :key 'car))))
     

(defun handle-redirect ()
  (let* ((script-name (hunchentoot:url-decode (tbnl:script-name*)))
         (url (gethash script-name *redirect-table*)))
    (cond ((stringp url)
           (redirect url))
          ((consp url)
           (multiple-value-bind (prefix term)
               (parse-script-name script-name)
             (multiple-choice prefix term url)))
          (t
           (setf (tbnl:return-code*) 404)
           (format nil "Not found - ~A"
                   (html-template:escape-string-all script-name))))))

(defvar *do-nothing* (constantly nil))

(defun map-sequence-groups (sequence
                            each-item
                            &key
                            (test 'eql)
                            (group-by 'identity)
                            (before-all *do-nothing*)
                            (before-group *do-nothing*)
                            (after-group *do-nothing*)
                            (after-all *do-nothing*))
  (funcall before-all)
  (let ((pre t)
        (last nil))
    (funcall before-all)
    (map nil
         (lambda (item)
           (cond (pre
                  (setf pre nil)
                  (setf last (funcall group-by item))
                  (funcall before-group last)
                  (funcall each-item item))
                 (t
                  (let ((current (funcall group-by item)))
                    (when (not (funcall test current last))
                      (funcall after-group last)
                      (funcall before-group current)
                      (setf last current))
                    (funcall each-item item)))))
         sequence)
    (funcall after-group last)
    (funcall after-all)))
       
(defun boldify (term string)
  (let* ((start (search term string :test 'string-equal))
         (end (and start (+ start (length term)))))
    (if end
        (concatenate 'string
                     (subseq string 0 start)
                     "<b>"
                     (subseq string start (+ start (length term)))
                     "</b>"
                     (subseq string end))
        string)))

(defun search-results-page (term results)
  (if (null results)
      "<i>No results found</i>"
      (with-output-to-string (stream)
        (map-sequence-groups results
                             (lambda (item)
                               (destructuring-bind (path . urls)
                                   item
                                 (dolist (url (if (consp urls)
                                                  (mapcar 'cdr urls)
                                                  (list urls)))
                                   (format stream "<li><tt><a href='~A'>~A</a></tt></li>"
                                           (html-template:escape-string-all url)
                                           (boldify term path)))))
                             :test 'string=
                             :before-group (lambda (x)
                                             (declare (ignore x))
                                             (format stream "<ul>"))
                             :after-group (lambda (x)
                                             (declare (ignore x))
                                             (format stream "</ul>"))
                             :group-by (lambda (item)
                                         (parse-script-name (car item)))))))

(defun search-template-page (term results)
  (let ((html-template:*warn-on-creation* nil)
        (html-template:*string-modifier* #'identity))
    (with-output-to-string (stream)
      (html-template:fill-and-print-template
       (merge-pathnames "templates/search-output.html"
                        *base-directory*)
       (list :escaped-term (tbnl:escape-for-html term)
             :result-count (length results)
             :no-results (null results)
             :results (search-results-page term results))
       :stream stream))))

(defun empty-search-template-page ()
  (let ((html-template:*warn-on-creation* nil)
        (html-template:*string-modifier* #'identity))
    (with-output-to-string (stream)
      (html-template:fill-and-print-template
       (merge-pathnames "templates/search.html"
                        *base-directory*)
       nil
       :stream stream))))

(defun handle-search ()
  (let ((query-term (tbnl:get-parameter "q")))
    (multiple-value-bind (prefix path-term)
        (parse-script-name (hunchentoot:url-decode (tbnl:script-name*)))
      (declare (ignore prefix))
      (let* ((term (or query-term path-term))
             (results (and term (urlsearch term))))
        (if term
            (search-template-page term results)
            (empty-search-template-page))))))
    
(defun starts-with (prefix string)
  (when (<= (length prefix) (length string))
    (string= prefix string :end2 (length prefix))))

(defun handle-robots ()
  (setf (tbnl:content-type*) "text/plain") 
  (format nil "User-agent: *~%Disallow: /search~%"))

(defun dispatch-handler (request)
  (cond ((starts-with "/search" (tbnl:script-name request))
         'handle-search)
        ((string= (tbnl:script-name request) "/robots.txt")
         'handle-robots)
        (t
         'handle-redirect)))

;;; Loading redirect files

(defun read-word (stream)
  "Read a \"word\" from STREAM, where a word is either a double-quoted
  string or a consecutive sequence of non-whitespace characters. In
  either case, a backslash may be used to precede any character, which
  will become part of the word."
  (let ((start (peek-char t stream nil))
        (delimiter #\Space))
    (case start
      (#\" (setf delimiter (read-char stream)))
      ((nil) (return-from read-word nil)))
    (with-output-to-string (out)
      (loop
       (let ((char (read-char stream nil)))
         (cond ((null char)
                (return))
               ((eql char #\\)
                (write-char (read-char stream) out))
               ((eql char delimiter)
                (return))
               (t
                (write-char char out))))))))

(defun line-words (line)
  (with-input-from-string (stream line)
    (loop for word = (read-word stream) while word collect word)))

(defun load-table (file)
  (let ((table (make-hash-table :test 'equal)))
    (flet ((save (key value)
             (setf (gethash key table) value))
           (save-pair (key k v)
             (push (cons k v) (gethash key table nil))))
      (with-open-file (stream file)
        (loop for line = (read-line stream nil)
              while line do
              (let ((words (line-words line)))
                (cond ((= (length words) 2)
                       (destructuring-bind (prefix url)
                           words
                         (save (format nil "/~A" prefix) url)
                         (save (format nil "/~A/" prefix) url)))
                      (t
                       (destructuring-bind (prefix sym type url)
                           words
                         (save-pair (format nil "/~A/~A" prefix sym)
                                    type url))))))))
    ;; collapse single-entry alists into a straight URL
    (maphash (lambda (k v)
               (when (and (consp v) (null (cdr v)))
                 (setf (gethash k table) (cdar v))))
             table)
    table))

(defun merge-tables (from to)
  (maphash (lambda (k v) (setf (gethash k to) v)) from))

(defparameter *default-redirect-defaults*
  (merge-pathnames "redirects/"
                   *base-directory*))

(defun load-redirects (file)
  (let ((table (load-table (merge-pathnames file
                                            *default-redirect-defaults*))))
    (merge-tables table *redirect-table*)))


(defun static-file-handler (request)
  (let ((base (merge-pathnames #p"static/" *base-directory*))
        (path (tbnl:script-name request)))
    (flet ((static-handler (file)
             (lambda ()
               (tbnl:handle-static-file (merge-pathnames file base)))))
      (cond ((string= path "/")
             (static-handler "index.html"))
            ((string= path "/robots.txt")
             (static-handler "robots.txt"))
            ((string= path "/style.css")
             (static-handler "style.css"))))))

;;; Startup

(defun install-handler ()
  (pushnew 'dispatch-handler tbnl:*dispatch-table*)
  (pushnew 'shortener-dispatcher tbnl:*dispatch-table*)
  (pushnew 'static-file-handler tbnl:*dispatch-table*))

(defun load-all-redirects ()
  (dolist (file (directory (merge-pathnames "*.txt"
                                            *default-redirect-defaults*)))
    (load-redirects file)))

(defun start ()
  (install-handler)
  (load-all-redirects)
  (swank:create-server :port *swank-port*
                       :dont-close t)
  (setf *web-server*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                          :access-log-destination nil
                                          :message-log-destination nil
                                          :port *web-port*))))

(defun stop ()
  (hunchentoot:stop *web-server*)
  (swank:stop-server *swank-port*))



;;; For buildapp startup

(defun main (argv)
  (declare (ignore argv))
  (load "/opt/l1sp/etc/init.lisp")
  (sb-impl::toplevel-repl nil))

  
