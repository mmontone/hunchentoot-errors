(defpackage :hunchentoot-trace
  (:use :cl :hunchentoot)
  (:local-nicknames
   (:hunch :hunchentoot))
  (:export
   :trace-acceptor
   :*trace-requests*
   :*trace-session*))

(in-package :hunchentoot-trace)

(defvar *trace-requests* t)
(defvar *trace-session* nil)

(defclass trace-acceptor (acceptor)
  ())

(defgeneric print-request (request format stream)
  (:documentation "Prints REQUEST to STREAM in FORMAT"))

(defmethod print-request ((request request) format stream)
  (prin1 request stream))

(defgeneric print-session (session format stream)
  (:documentation "Prints SESSION to STREAM in FORMAT"))

(defmethod print-session ((session session) format stream)
  (prin1 session stream))

(defmethod print-session ((session session) (format (eql :text-log)) stream)
  (loop for (key . value) in (hunchentoot::session-data session)
        do (format stream "  ~s: ~s~%" key value)))

(defmethod print-request ((request request) (format (eql :text-log)) stream)
  (format stream "  uri: ~a~%" (request-uri request))
  (format stream "  method: ~a~%" (request-method request))
  (format stream "  headers:~%")
  (loop for (key . value) in (hunchentoot:headers-in request)
        do (format stream "    ~a: ~a~%" key value))
  (when (member (request-method request) '(:patch :post))
    (format stream "  post parameters:~%")
    (loop for (key . value) in (hunchentoot:post-parameters request)
          do (format stream "    ~a: ~a~%" key value))
    (format stream "  request body:~%")
    (write-string (hunchentoot:raw-post-data :request request :force-text t) stream)
    (terpri stream)))

(defmethod print-request ((request request) (format (eql :html)) stream)
  (format stream "<div class=\"request\">")
  (format stream "<h1>Request</h1>~%")
  (format stream "<p><b>Uri:</b> ~a</p>~%" (request-uri request))
  (format stream "<p><b>Method:</b> ~a</p>~%" (request-method request))
  (format stream "<p><b>Headers:</b>~%<ul>")
  (loop for (key . value) in (hunchentoot:headers-in request)
        do (format stream "<li><i>~a:</i> ~a</li>~%" key value))
  (format stream "</ul></p>")
  (when (member (request-method request) '(:patch :post))
    (format stream "<p><b>Post parameters:</b>~%<ul>")
    (loop for (key . value) in (hunchentoot:post-parameters request)
          do (format stream "<li><i>~a:</i> ~a</li>~%" key value))
    (format stream "</ul></p>"))
  (format stream "</div>"))

(defmethod print-session ((session session) (format (eql :html)) stream)
  (format stream "<div class=\"session\">")
  (format stream "<h1>Session</h1>~%<ul>")
  (loop for (key . value) in (hunchentoot::session-data session)
        do (format stream "<li>~s: ~s~%</li>" key value))
  (format stream "</ul></div>"))

(defmethod acceptor-log-access ((acceptor trace-acceptor) &key return-code)
  (declare (ignore return-code))
  (call-next-method)
  (hunch::with-log-stream (stream (acceptor-access-log-destination acceptor) hunch::*access-log-lock*)
    (when *trace-requests*
      (print-request hunchentoot:*request* :text-log stream))
    (when *trace-session*
      (print-session hunchentoot:*session* :text-log stream))))

;; Example usage:

;; (defclass my-acceptor (hunchentoot:easy-acceptor hunchentoot-trace:trace-acceptor)
;;   ())

;; (hunchentoot:start (make-instance 'my-acceptor :port 5000))


