;; Signal and handle HTTP errors

;; Example usage:

;; (hunchentoot-errors:with-http-error-handler
;;   (hunchentoot-errors:http-error hunchentoot:+http-bad-request+
;;                                  "Bad argument: FOO"))

(in-package :hunchentoot-errors)

(define-condition http-error (simple-error)
  ((http-status-code :initarg :http-status-code
                     :accessor http-status-code
                     :type integer
                     :initform hunchentoot:+http-internal-server-error+))
  (:documentation "An error condition that to be handled by Hunchentoot as HUNCHENTOOT:RETURN-CODE of reply."))

(defun http-error (http-status-code errormsg &rest args)
  "Signal an HTTP-ERROR condition."
  (error 'http-error :http-status-code http-status-code
                     :format-control errormsg
                     :format-arguments args))

(defun call-with-http-error-handler (func)
  "Call function with an HTTP error handler."
  (handler-case
      (funcall func)
    (http-error (http-error)
      (setf (hunchentoot:return-code*)
            (http-status-code http-error))
      (apply #'format
             (simple-condition-format-control http-error)
             (simple-condition-format-arguments http-error)))))

(defmacro with-http-error-handler (&body body)
  "Wrap BODY with an HTTP-ERROR handler.

Example usage:

(hunchentoot-errors:with-http-error-handler
  (hunchentoot-errors:http-error hunchentoot:+http-bad-request+
                     \"Bad argument: FOO\"))
"
  `(call-with-http-error-handler (lambda () ,@body)))
