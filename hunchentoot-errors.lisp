(in-package #:hunchentoot-errors)

(defclass errors-acceptor (acceptor)
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
        do (format stream "  ~a: ~s~%" key value)))

(defmethod print-request ((request request) (format (eql :text-log)) stream)
  (format stream "  uri: ~a~%" (request-uri request))
  (format stream "  method: ~a~%" (request-method request))
  (when (member (request-method request) '(:patch :post))
    (format stream "  post parameters:~%")
    (loop for (key . value) in (hunchentoot:post-parameters request)
          do (format stream "    ~a: ~a~%" key value))))

(defmethod print-request ((request request) (format (eql :html)) stream)
  (format stream "<div class=\"request\">")
  (format stream "<h1>Request</h1>~%")
  (format stream "<p><b>Uri:</b> ~a</p>~%" (request-uri request))
  (format stream "<p><b>Method:</b> ~a</p>~%" (request-method request))
  (when (member (request-method request) '(:patch :post))
    (format stream "<p><b>Post parameters:</b>~%<ul>")
    (loop for (key . value) in (hunchentoot:post-parameters request)
          do (format stream "<li><i>~a:</i> ~a</li>" key value))
    (format stream "</ul></p>"))
  (format stream "</div>"))

(defmethod print-session ((session session) (format (eql :html)) stream)
  (format stream "<div class=\"session\">")
  (format stream "<h1>Session</h1>~%<ul>")
  (loop for (key . value) in (hunchentoot::session-data session)
        do (format stream "<li>~s: ~s~%</li>" key value))
  (format stream "</ul></div>"))

(defun accept-format (&optional (content-type (hunchentoot:header-in "accept" *request*)))
  (or (and content-type
           (let ((accepts (best-match
                           (list "text/lisp"
                                 "application/lisp"
                                 "text/xml"
                                 "application/xml"
                                 "text/html"
                                 "application/json")
                           content-type)))
             (string-case:string-case (accepts :default :text)
               ("text/xml" :xml)
               ("application/xml" :xml)
               ("text/html" :html)
               ("application/json" :json)
               ("text/lisp" :sexp)
               ("application/lisp" :sexp))))
      :text))

(defgeneric acceptor-log-error (stream acceptor log-level format-string &rest format-arguments))

(defmethod acceptor-log-error (stream (acceptor errors-acceptor) log-level format-string &rest format-arguments)
  (format stream "[~A~@[ [~A]~]] ~?~%"
          (hunchentoot::iso-time) log-level
          format-string format-arguments)
  (format stream "HTTP REQUEST:~%")
  (print-request *request* :text-log stream)
  (format stream "~%SESSION:~%")
  (print-session *session* :text-log stream)
  (terpri stream))

(defmethod acceptor-log-message ((acceptor errors-acceptor) log-level format-string &rest format-arguments)
  "Sends a formatted message to the destination denoted by (ACCEPTOR-MESSAGE-LOG-DESTINATION ACCEPTOR).
FORMAT and ARGS are as in FORMAT.
LOG-LEVEL is a keyword denoting the log level or NIL in which case it is ignored."
  (hunchentoot::with-log-stream (stream (acceptor-message-log-destination acceptor) hunchentoot::*message-log-lock*)
    (handler-case
        (apply #'acceptor-log-error stream acceptor log-level format-string format-arguments)
      (error (e)
        (ignore-errors
         (format *trace-output* "error ~A while writing to error log, error not logged~%" e))))))

(defmethod acceptor-status-message ((acceptor errors-acceptor) http-status-code &key &allow-other-keys)
  (concatenate 'string (call-next-method)
    (if *show-lisp-errors-p*
      (with-output-to-string (msg)
        (let ((format (accept-format)))
          (print-request *request* format msg)
          (print-session *session* format msg))))
    ""))
