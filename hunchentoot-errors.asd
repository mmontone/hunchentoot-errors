(asdf:defsystem #:hunchentoot-errors
  :description "Augments Hunchentoot error pages and logs with request and session information"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :homepage "https://github.com/mmontone/hunchentoot-errors"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:string-case #:parse-number)
  :components ((:file "package")
               (:file "mimeparse")
               (:file "hunchentoot-errors")))
