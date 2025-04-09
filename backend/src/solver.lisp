(defpackage :solver-core
  (:use :cl)
  (:import-from :wordle-guess
                #:guess-char
                #:guess-color)
  (:export #:determine-suggestions))

(in-package :solver-core)

(defun determine-suggestions (words guess-history freq-table)
  "Return candidate words sorted by frequency (descending) if known, then others."
  (let ((candidates (get-candidates words guess-history)))
    (multiple-value-bind (freq-known freq-unknown)
        (loop for w in candidates
              if (gethash w freq-table)
                collect w into freq-known
              else
                collect w into freq-unknown
              finally (return (values freq-known freq-unknown)))
      (setf freq-known (sort freq-known #'>
                             :key (lambda (w)
                                    (gethash w freq-table))))
      (append freq-known freq-unknown))))

(defun get-candidates (words guess-history)
  "Return a list of candidates by filtering WORDS based on GUESS-HISTORY."
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
