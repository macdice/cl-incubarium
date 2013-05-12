(in-package :incubarium)

(defun ga-example-onemax-fitness (genotype)
  "The fitness function for the one-max problem counts ones in GENOTYPE."
  (loop for codon in genotype sum codon))

(defun ga-example-onemax ()
  "Search for a solution to the one-max problem."
  ;; The goal is to find geotypes which have the largest number of
  ;; bits lit.  The best solution is (1 1 1 1 1 1 1 1), the worst
  ;; solution is (0 0 0 0 0 0 0 0), and we have to figure that out
  ;; using GA.  Usually seems to converge on the best solution in
  ;; under 10 generations.
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
        do (format t "best = ~A, fitness = ~a, worst = ~A, fitness = ~a~%"
                   (individual-genotype (first population))
                   (individual-fitness (first population))
                   (individual-genotype (car (last population)))
                   (individual-fitness (car (last population))))))

(defun square (x)
  (* x x))

(defun gp-example-polynomial-fitness (individual)
  (let ((fun 
         (eval `(lambda (x) 
                  (declare (ignorable x))
                  ,individual))))
    (/ 1.0 
       (+ 1 
          (loop 
             for (x expected) in '((-15 862)
                                   (1 14)
                                   (10 437)
                                   (42 7189)
                                   (100 40307))
             sum (square (- (funcall fun x) expected)))))))

(defun gp-example-polynomial ()
  "Fit a function composed from the basic arithmetic operators to a
set of 5 data points."
  ;; We want to find a function that is equivalent to:
  ;;           
  ;; f(x) = 4x^2 + 3x + 7
  ;;
  ;; We use the following 'training' data:
  ;;
  ;;     x    f(x)
  ;; ------+-------
  ;;   -15 |   862
  ;;     1 |    14
  ;;    10 |   437
  ;;    42 |  7189
  ;;   100 | 40307
  (loop 
     for generation from 1 to 300
     for population = (evaluate-population
                       (ramped-population 40 '((+ . 2) (- . 2) (* . 2)) '((:random 0 10) x) 2 5)
                       #'gp-example-polynomial-fitness)
     then (evaluate-population
           (tournament population
                       #'tree-random-recombine
                       0.9
                       #'identity
                       0.0)
           #'gp-example-polynomial-fitness)
     do (format t "generation = ~a, best = ~a, fitness = ~a~%"
                generation
                (individual-genotype (first population))
                (individual-fitness (first population)))
     while (< (individual-fitness (first population)) 0.9999)))
  
  