(defpackage :solver-utils
  (:use :cl)
  (:import-from :hunchentoot
                #:return-code*)
  (:import-from :cl-json
                #:encode-json-to-string)
  (:import-from :cl-ppcre
                #:split)
  (:export #:read-file-lines
           #:load-word-frequencies
           #:alist-to-plist
           #:json-error))

(in-package :solver-utils)

(defun read-file-lines (relpath)
  (let ((file (asdf:system-relative-pathname :wordle-solver relpath)))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line
            collect line))))

(defun load-word-frequencies (relpath)
  "Load word frequencies from RELPATH and return a new hash table."
  (let ((freq-table (make-hash-table :test #'equal))
        (file (asdf:system-relative-pathname :wordle-solver relpath)))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line
            do (destructuring-bind (word freq)
                   (split "," line)
                 (setf (gethash word freq-table) (parse-integer freq)))))
    freq-table))

(defun alist-to-plist (alist)
  "Convert an association list to a property list."
  (loop for (key . value) in alist
        collect (intern (string-upcase key) :keyword)
        collect value))

(defun json-error (status-code message)
  "Return a JSON error response with STATUS-CODE and MESSAGE."
  (setf (return-code*) status-code)
  (encode-json-to-string `(:error ,message)))

