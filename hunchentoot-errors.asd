(asdf:defsystem #:hunchentoot-errors
  :description "Augments Hunchentoot error pages and logs with request and session information"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:string-case #:parse-number)
  :components ((:file "package")
               (:file "mimeparse")
               (:file "hunchentoot-errors")))
