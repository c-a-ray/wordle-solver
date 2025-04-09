(asdf:defsystem :wordle-solver
  :name "Wordle Solver"
  :version "0.1"
  :author "Cody Ray"
  :license "MIT"
  :depends-on (:hunchentoot :cl-json :cl-csv :cl-ppcre)
  :serial t
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "wordle-guess")
                 (:file "solver" :depends-on ("wordle-guess"))
                 (:file "server" :depends-on ("wordle-guess" "solver" "utils"))))))
