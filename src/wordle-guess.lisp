(defpackage :wordle-guess
  (:use :cl)
  (:export #:wordle-guess
           #:make-wordle-guess
           #:guess-char
           #:guess-color
           #:correct-guess-p
           #:print-object))

(in-package :wordle-guess)

(defclass wordle-guess ()
  ((word :initarg :word
         :accessor word
         :type string
         :documentation "The guessed word.")
   (colors :initarg :colors
           :accessor colors
           :type list
           :documentation "The feedback for the guess.")))

(defmethod make-wordle-guess ((word string) (colors list))
  "Create a WORDLE-GUESS instance."
  (make-instance 'wordle-guess
                 :word word
                 :colors (mapcar (lambda (color)
                                   (intern (string-upcase color) "KEYWORD"))
                                 colors)))

(defmethod initialize-instance :after ((guess wordle-guess) &key word colors)
  "Validate WORD and COLORS after initializing the instance."
  (unless (and (stringp word)
               (= (length word) 5))
    (error "Invalid word: ~A. It must be a 5-letter string." word))
  (unless (and (listp colors)
               (= (length colors) 5)
               (every (lambda (color)
                        (member color '(:green :yellow :gray)))
                      colors))
    (error "Invalid colors: ~A. Must be a list of 5 valid colors (:green, :yellow, :gray)." colors)))

(defmethod guess-char ((guess wordle-guess) pos)
  "Return the character at position POS in the guessed word."
  (char (word guess) pos))

(defmethod guess-color ((guess wordle-guess) pos)
  "Return the color at position POS in the feedback colors."
  (nth pos (colors guess)))

(defmethod correct-guess-p ((guess wordle-guess))
  "Return T if all feedback colors are :green."
  (every (lambda (color) (eq color :green)) (colors guess)))

(defmethod print-object ((guess wordle-guess) stream)
  "Pretty print a WORDLE-GUESS instance."
  (print-unreadable-object (guess stream :type t :identity nil)
    (format stream "~A ~A" (word guess) (colors guess))))
