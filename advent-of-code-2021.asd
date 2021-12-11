(asdf:defsystem #:advent-of-code-2021
  :description "Advent of Code 2021"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("alexandria" "cl-ppcre" "fare-memoization" "array-operations" "infix-math")
  :components ((:file "src/utils")
               (:file "src/day-01" :depends-on ("src/utils"))
               (:file "src/day-02" :depends-on ("src/utils"))
               (:file "src/day-03" :depends-on ("src/utils"))
               (:file "src/day-04" :depends-on ("src/utils"))
               (:file "src/day-05" :depends-on ("src/utils"))
               (:file "src/day-06" :depends-on ("src/utils"))
               (:file "src/day-07" :depends-on ("src/utils"))
               (:file "src/day-08" :depends-on ("src/utils"))
               (:file "src/day-09" :depends-on ("src/utils"))
               (:file "src/day-10" :depends-on ("src/utils"))
               (:file "src/day-11" :depends-on ("src/utils"))))
