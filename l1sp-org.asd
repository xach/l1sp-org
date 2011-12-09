;;;; redirector.asd

(asdf:defsystem #:l1sp-org
  :description "The l1sp.org website and data."
  :author "Zach Beane <xach@xach.com>"
  :license "BSD-style"
  :depends-on (#:hunchentoot
               #:cl-ppcre
               #:cl-who
               #:babel
               #:ironclad
               #:html-template)
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "redirector")
               (:file "shortener")
               (:file "scrapers")))

(defpackage #:l1sp-org-config
  (:documentation "Configuration variables for the l1sp-org package.")
  (:use)
  (:export #:*base-directory*))

(defvar l1sp-org-config:*base-directory*
  (make-pathname :type nil
                 :name nil
                 :defaults *load-truename*)
  "The base directory for templates, data files, etc.")
