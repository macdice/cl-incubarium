(defpackage :incubarium
  (:use :common-lisp)
  (:export :make-population
           :ramped-population
           :grow-population
           :full-population
           :evaluate-population
           :individual-genotype
           :individual-fitness
           :evaluate-population
           :tournament
           :genotype->phenotype)
  (:documentation "The Incubarium toy AI library."))
