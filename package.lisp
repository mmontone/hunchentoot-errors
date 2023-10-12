(defpackage #:hunchentoot-errors
  (:use #:cl #:hunchentoot)
  (:export
   #:errors-acceptor
   #:trace-acceptor
   #:http-error
   #:call-http-error-handler
   #:with-http-error-handler))
