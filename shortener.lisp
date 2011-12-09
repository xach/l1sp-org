;;;; shortener.lisp

(in-package #:l1sp-org)

;;;; url shortener service

(defparameter *shortener-storage-path*
  (merge-pathnames "shorturl/" *base-directory*))

(defparameter *shortener-counter*
  (merge-pathnames "shorturl/counter.dat" *base-directory*))

(defparameter *counter-lock* (sb-thread:make-mutex :name "shorturl"))

(defparameter *shortener-password* "x")

(defun read-file-u32 (file)
  (if (probe-file file)
      (with-open-file (stream file
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (+ (ash (read-byte stream) 24)
           (ash (read-byte stream) 16)
           (ash (read-byte stream)  8)
           (ash (read-byte stream)  0)))
      (random 1000)))

(defun write-file-u32 (value file)
  (ensure-directories-exist file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-byte (ldb (byte 8 24) value) stream)
    (write-byte (ldb (byte 8 16) value) stream)
    (write-byte (ldb (byte 8  8) value) stream)
    (write-byte (ldb (byte 8  0) value) stream)
    value))

(defun next-counter ()
  (block nil
    (sb-thread:with-mutex (*counter-lock*)
      (let ((new-value (+ (read-file-u32 *shortener-counter*)
                          (1+ (random 249)))))
        (write-file-u32 new-value *shortener-counter*)))))

(defun key-file (key)
  (flet ((endref (i)
           (aref key (mod i (length key)))))
    (let ((a (string (endref -1)))
          (b (string (endref -2))))
      (merge-pathnames (make-pathname :name key
                                      :type "txt"
                                      :directory (list :relative a b))
                       *shortener-storage-path*))))
(defun next-key ()
  (string-downcase (write-to-string (next-counter) :base 36)))

(defun key-value (key)
  (let ((file (key-file key)))
    (with-open-file (stream file)
      (read-line stream))))

(defun (setf key-value) (new-value key)
  (let ((file (key-file key)))
    (ensure-directories-exist file)
    (with-open-file (stream file
                            :direction :output
                            :if-exists :error)
      (write-line new-value stream))))

(defun url-path-key (path)
  (unless (starts-with "/r/" path)
    (error "Not a shortener url"))
  (subseq path 3))

(defun key-url (key)
  (or (ignore-errors (key-value key))
      "http://l1sp.org/html/"))

(defun string-md5 (string)
  (with-output-to-string (stream)
    (map nil (lambda (code)
               (format stream "~(~2,'0X~)" code))
         (sb-md5:md5sum-string string))))

(defun url-digest-file-name (url)
  (let* ((digest (string-md5 url))
         (a (string (aref digest 0)))
         (b (string (aref digest 1))))
    (merge-pathnames (make-pathname :name digest
                                    :type "dat"
                                    :directory (list :relative "url-keys" a b))
                     *shortener-storage-path*)))

(defun call-with-temporary-output-file (template fun)
  (let* ((counter 0)
         file
         stream)
    (tagbody
     retry
       (setf file (format nil "~A-tmp-~6,'0X"
                          (namestring template)
                          (incf counter)))
       (unwind-protect
            (progn
              (setf stream (open file :direction :output :if-exists nil))
              (unless stream
                (go retry))
              (funcall fun stream))
         (when stream
           (close stream))))
    file))

(defun call-with-atomic-output (file fun)
  (let (temp-file)
    (unwind-protect
         (progn
           (setf temp-file (call-with-temporary-output-file file fun))
           (rename-file temp-file file))
      (when temp-file
        (ignore-errors (delete-file temp-file))))))

(defun find-url-key (url)
  (with-open-file (stream (url-digest-file-name url)
                          :if-does-not-exist nil)
    (when stream
      (read-line stream))))

(defun store-url-key (url)
  (let ((key (next-key))
        (pathname (url-digest-file-name url)))
    (setf (key-value key) url)
    (ensure-directories-exist pathname)
    (call-with-atomic-output
     pathname
     (lambda (stream)
       (write-line key stream)))
    key))

(defun ensure-url-key (url)
  (or (find-url-key url)
      (store-url-key url)))

(defun handle-shortened ()
  (case (tbnl:request-method*)
    (:get
     (let ((new-location (key-url (url-path-key (tbnl:script-name*)))))
       (setf (tbnl:return-code*) 301)
       (setf (tbnl:header-out "Location") new-location)
       ""))
    (:post
     (let ((url (tbnl:post-parameter "url"))
           (password (tbnl:post-parameter "x")))
       (when (equal password *shortener-password*)
         (let ((key (ensure-url-key url)))
           (setf (tbnl:content-type*) "text/plain")
           (format nil "http://l1sp.org/r/~A" key)))))))

(defun shortener-dispatcher (request)
  (when (starts-with "/r/" (tbnl:script-name request))
    'handle-shortened))

