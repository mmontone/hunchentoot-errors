(in-package #:hunchentoot-errors)

(defclass errors-acceptor (acceptor)
  ((log-request :initarg :log-request
                :accessor log-requestp
                :initform t
                :documentation "When enabled, request information is written to the log.")
   (debug-request :initarg :debug-request
                  :accessor debug-requestp
                  :initform t
                  :documentation "When enabled, request information is printed in Hunchentoot status error pages.")
   (log-session :initarg :log-session
                :accessor log-sessionp
                :initform t
                :documentation "When enabled, session information is written to the log.")
   (debug-session :initarg :debug-session
                  :accessor debug-sessionp
                  :initform t
                  :documentation "When enabled, session information is printed in Hunchentoot status error pages."))
  (:documentation "Subclasses of this acceptor augment Hunchentoot error pages and logs with request and session information."))

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
          do (format stream "    ~a: ~a~%" key value))))

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
        do (format stream "<li>~s: <code>~s</code>~%</li>"
                   key (hunchentoot:escape-for-html (princ-to-string value))))
  (format stream "</ul></div>"))

(defun accept-format (&optional (content-type (hunchentoot:header-in "accept" *request*)))
  (or (and content-type
           (let ((accepts (mimeparse:best-match
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
  ;; This snippet is from original Hunchentoot:
  (format stream "[~A~@[ [~A]~]] ~?~%"
          (hunchentoot::iso-time) log-level
          format-string format-arguments)

  ;; This part is hunchentoot-errors specific:
  (when (and (log-requestp acceptor)
             (boundp '*request*))
    (format stream "HTTP REQUEST:~%")
    (print-request *request* :text-log stream))
  (when (and (log-sessionp acceptor)
             (boundp '*session*)
             (not (null *session*)))
    (format stream "~%SESSION:~%")
    (print-session *session* :text-log stream))
  (terpri stream))

(defmethod acceptor-log-message ((acceptor errors-acceptor) log-level format-string &rest format-arguments)
  "Sends a formatted message to the destination denoted by (ACCEPTOR-MESSAGE-LOG-DESTINATION ACCEPTOR).
FORMAT and ARGS are as in FORMAT.
LOG-LEVEL is a keyword denoting the log level or NIL in which case it is ignored."
  (if (not (eq log-level :error))
      (call-next-method)
      ;; else
      (hunchentoot::with-log-stream (stream (acceptor-message-log-destination acceptor) hunchentoot::*message-log-lock*)
	(handler-case
            (apply #'acceptor-log-error stream acceptor log-level format-string format-arguments)
	  (error (e)
            (ignore-errors
             (format *trace-output* "error ~A while writing to error log, error not logged~%" e)))))))

(defmethod acceptor-status-message ((acceptor errors-acceptor) http-status-code &key &allow-other-keys)
  (if (not *show-lisp-errors-p*)
      (call-next-method)
      (concatenate
       'string
       (call-next-method)
       (with-output-to-string (msg)
         (let ((format (accept-format)))
           (when (and (debug-requestp acceptor)
                      (boundp '*request*)
                      (not (null *request*)))
             (print-request *request* format msg))
           (when (and (debug-sessionp acceptor)
                      (boundp '*session*)
                      (not (null *session*)))
             (print-session *session* format msg)))))))
