;; Signal HTTP errors

(in-package :hunchentoot-errors)

(defun http-error (http-error &optional result)
  "Abort current handler and signal HTTP error HTTP-ERROR.

HTTP-ERROR should be an HTTP status code (integer)."
  (setf (hunchentoot:return-code*) http-error)
  (hunchentoot:abort-request-handler result))
