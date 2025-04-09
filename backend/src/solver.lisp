(defpackage :solver-core
  (:use :cl)
  (:import-from :wordle-guess
                #:guess-char
                #:guess-color)
  (:export #:determine-suggestions))

(in-package :solver-core)

(defun determine-suggestions (words guess-history)
  "Return a list of suggested guesses by filtering WORDS based on GUESS-HISTORY."
  (loop for word in words
        when (every (lambda (guess)
                      (possible-solution word guess))
                    (loop for (num guess) on guess-history by #'cddr
                          collect guess))
          collect word))

(defun possible-solution (word previous-guess)
  "Check if WORD is a possible solution based on PREVIOUS-GUESS."
  (loop for i from 0 below 5
        for guessed-char = (guess-char previous-guess i)
        for color = (guess-color previous-guess i)
        for word-char = (char word i)
        always (cond
                 ((eql color :green)
                  (char= guessed-char word-char))

                 ((eql color :yellow)
                  (and (not (char= guessed-char word-char))
                       (find guessed-char word)))

                 ((eql color :gray)
                  (not (find guessed-char word)))

                 (t nil))))
