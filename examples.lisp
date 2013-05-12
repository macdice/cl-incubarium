(in-package :incubarium)

(defun ga-example-onemax-fitness (genotype)
  "The fitness function for the one-max problem counts ones in GENOTYPE."
  (loop for codon in genotype sum codon))

(defun ga-example-onemax ()
  "Search for a solution to the one-max problem."
  ;; The goal is to find geotypes which have the largest number of
  ;; bits lit.  The best solution is (1 1 1 1 1 1 1 1), the worst
  ;; solution is (0 0 0 0 0 0 0 0), and we have to figure that out
  ;; using GA.
  (loop repeat 10
        for population = (evaluate-population
                          (make-population 10 0 2 8 8)
                          #'ga-example-onemax-fitness)
        then (evaluate-population
              (tournament population
                          #'one-point-crossover
                          0.8
                          (make-single-point-mutator 0 2)
                          0.2)
              #'ga-example-onemax-fitness)
        do (format t "worst = ~A, fitness = ~a, best = ~A, fitness = ~a~%"
                   (individual-genotype (first population))
                   (individual-fitness (first population))
                   (individual-genotype (car (last population)))
                   (individual-fitness (car (last population))))))

