(defpackage :solver-server
  (:use :cl)
  (:import-from :hunchentoot
                #:define-easy-handler
                #:easy-acceptor
                #:request-method*
                #:raw-post-data
                #:start
                #:stop
                #:start-session
                #:session-value)
  (:import-from :cl-json
                #:encode-json-to-string
                #:decode-json-from-string)
  (:import-from :solver-core
                #:determine-suggestions)
  (:import-from :solver-utils
                #:read-file-lines
                #:load-word-frequencies
                #:alist-to-plist
                #:json-error)
  (:import-from :wordle-guess
                #:make-wordle-guess)
  (:export #:start-server #:stop-server))

(in-package :solver-server)

(defparameter *server* nil
  "The running server instance.")

(defparameter *word-list* nil
  "The list of valid Wordle words.")

(defparameter *words-source* "data/words.txt"
  "Relative path to file containing valid Wordle word list.")

(defparameter *word-frequencies* nil
  "Hash table with Norvig word frequencies.")

(defparameter *word-frequency-source* "data/words-with-frequencies.csv"
  "Relative path to file containing a subset of *WORDS-SOURCE* words with frequencies.")

(defun start-server (&optional (port 8080))
  "Start the server on PORT (defaults to 8080)."
  (setf *word-list* (read-file-lines *words-source*))
  (setf *word-frequencies* (load-word-frequencies *word-frequency-source*))
  (setf *server* (make-instance 'easy-acceptor :port port))
  (start *server*)
  (format t "Server started on port ~D.~%" port))

(defun stop-server ()
  "Stop the server."
  (when *server*
    (stop *server*)
    (setf *server* nil)
    (format t "Server stopped.~%")))

(defun get-guess-history ()
  "Retrieve the guess history from the current session."
  (or (session-value :guess-history) '()))

(defun set-guess-history (history)
  "Set the guess history for the current session."
  (setf (session-value :guess-history) history))

(defun get-last-guess-number (history)
  "Return the number of the most recent guess (or 0 if no guesses have been made)."
  (if (null history)
      0
      (loop for (num _) on history by #'cddr
            maximize (if (and (keywordp num)
                              (let ((num-str (symbol-name num)))
                                (every #'digit-char-p (subseq num-str 1))))
                         (parse-integer (subseq (symbol-name num) 1))
                         0))))

(defun update-guess-history (guess guess-number)
  "Add GUESS to guess history at GUESS-NUMBER if it's the next in sequence."
  (let* ((history (get-guess-history))
         (last-guess (get-last-guess-number history)))
    (format t "Guess history before update: ~A~%" history)
    (format t "Last guess number: ~A~%" last-guess)
    (cond
      ((= guess-number (1+ last-guess))
       (let ((new-history (append history (list (intern (format nil ":~D" guess-number) :keyword) guess))))
         (format t "New guess history: ~A~%" new-history)
         (set-guess-history new-history)
         new-history))
      ((> guess-number (1+ last-guess))
       (error "Out of order guess: expected guess #~D but got #~D."
              (1+ last-guess) guess-number))
      (t
       (error "Duplicate or invalid guess number: ~D" guess-number)))))

(define-easy-handler (reset :uri "/reset") ()
  "Reset the game and reload the word list."
  (setf *word-list* (read-file-lines *words-source*))
  (set-guess-history '())
  (encode-json-to-string '(:status "game reset")))

(define-easy-handler (guess :uri "/guess") ()
  "Handle a user's guess."
  (start-session)
  (format t "Guess History: ~A~%" (get-guess-history))
  (cond
    ((eq (request-method*) :post)
     (let* ((raw-body (raw-post-data)))
       (if raw-body
           (let* ((body (flexi-streams:octets-to-string raw-body :external-format :utf-8))
                  (data (decode-json-from-string body))
                  (plist (alist-to-plist data))
                  (word (getf plist :word))
                  (colors (getf plist :colors)))
             (handler-case
                 (let* ((guess-number (getf plist :guess-number))
                        (guess (make-wordle-guess word colors))
                        (suggestions (determine-suggestions *word-list*
                                                            (update-guess-history guess guess-number)
                                                            *word-frequencies*)))
                   (encode-json-to-string suggestions))
               (error (err)
                 (json-error 400 (format nil "~A" err)))))
           (json-error 400 "Missing request body"))))
    (t
     (json-error 405 "Method not allowed"))))
