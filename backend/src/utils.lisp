(defpackage :solver-utils
  (:use :cl)
  (:import-from :hunchentoot
                #:return-code*)
  (:import-from :cl-json
                #:encode-json-to-string)
  (:export #:read-file-lines
           #:alist-to-plist
           #:json-error))

(in-package :solver-utils)

(defun read-file-lines (relative-path)
  (let ((file (asdf:system-relative-pathname :wordle-solver relative-path)))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line
            collect line))))

(defun alist-to-plist (alist)
  "Convert an association list to a property list."
  (loop for (key . value) in alist
        collect (intern (string-upcase key) :keyword)
        collect value))

(defun json-error (status-code message)
  "Return a JSON error response with STATUS-CODE and MESSAGE."
  (setf (return-code*) status-code)
  (encode-json-to-string `(:error ,message)))
