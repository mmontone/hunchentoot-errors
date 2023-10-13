(defpackage #:hunchentoot-errors
  (:use #:cl #:hunchentoot)
  (:export
   #:errors-acceptor
   #:trace-acceptor
   #:http-error))
