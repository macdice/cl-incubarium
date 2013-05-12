(defpackage :incubarium-asd
  (:use :cl :asdf))

(in-package :incubarium-asd)

(defsystem incubarium
  :name "Incubarium"
  :version "0.0.0"
  :description "Toy AI exercises."
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "genetic-algorithm")
               (:file "genetic-programming")
               (:file "grammatical-evolution")))
