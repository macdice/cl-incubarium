;;; INCUBARIUM
;; (c) 2010-2013 Thomas Munro <munro@ip9.org>

;; This is a toy genetic algorithm system.  It is uses lists for
;; genomes and populations, so it is probably not very efficient!
;; Both could probably be represented by arrays and modified in place.

;; REFERENCES
;;
;; * http://en.wikipedia.org/wiki/Genetic_algorithm
;; * http://en.wikipedia.org/wiki/Crossover_(genetic_algorithm)

(in-package :incubarium)

;;; OPERATIONS ON GENOTYPES

(defun make-random-genotype (min-value max-value min-size max-size)
  "Generate a new random genotype (aka chromosomes).
The genotype is a list of values chosen randomly from the
interval [MIN-VALUE, MAX-VALUE), of floating point type if either
of those values is a floating point number, and integer type
otherwise.  The size of the list is randomly chosen from the
inverval [MIN-SIZE, MAX-SIZE)."
  (let ((value-range (- max-value min-value))
        (size (if (>= min-size max-size)
                  min-size
                  (+ min-size (random (- max-size min-size))))))
    (if (or (floatp min-value) (floatp max-value))
        (loop repeat size collect
              (+ min-value (* value-range (/ (random 1000000) 1000000.0))))
        (loop repeat size collect (+ min-value (random value-range))))))

(defun one-point-crossover (male female)
  "Return two new genotypes which result from combining MALE and FEMALE.
The two parent genotypes are split at a single random point, as
illustrated:

               +---------- randomly chosen point for split
               |
  Male:    (1 0|1 1 1)
  Female:  (0 0|0 1 1)
               |
  Child 1: (1 0|0 1 1) --- head of male, tail of female
  Child 2: (0 0|1 1 1) --- head of female, tail of male
               |

This operation works best if MALE and FEMALE are of the same length,
but it can handle geotypes of differing lengths.  The two children are
returned as a multiple values.  Always takes at least one element from
each chromosome."
  (let ((point (1+ (random (- (max (length male) (length female)) 1)))))
    (values (append (take male point) (drop female point))
            (append (take female point) (drop male point)))))

(defun cut-and-splice (male female)
  "Return two genotypes which result from combining MALE and FEMALE.
The two parent genotypes are split at two randomly chosen points,
as illustrated:

               +---------- randomly chosen point for split
               |
  Male:    (1 0|1 1 1)
  Female:  (0 0 0|1 1)
                 |
                 +-------- randomly chosen point for split

  Child 1: (1 0|1 1)   --- head of male, tail of female
  Child 2: (0 0 0|1 1 1) - head of female, tail of male

It does not matter if the two chromosomes are of different lengths,
and the child chromosomes can be of any length from two to the
combined length of MALE and FEMALE minus two.  At least one element is
kept from each chromosome."
  (let ((male-point (1+ (random (- (length male) 1))))
        (female-point (1+ (random (- (length female) 1)))))
    (values (append (take male male-point) (drop female female-point))
            (append (take female female-point) (drop male male-point)))))

;; TODO
;; * two-point-crossover
;; * uniform-crossover
;; * half-uniform-crossover
;; * ordered-crossover (for the travelling salesman problem!)
;; * edge-recombination-crossover
;; * partially-mapped-crossover

(defun make-single-point-mutator (min-value max-value)
  "Make a function to randomise one integer to [MIN-VALUE, MAX-VALUE)."
  (lambda (genotype)
    (let ((point (random (length genotype))))
      (append (take genotype point)
              (list (+ min-value (random (- max-value min-value))))
              (drop genotype (1+ point))))))

;; TODO
;; * more kinds of mutation, support for real values etc
;; * element swapping, ...

;; INDIVIDUALS

(defstruct individual
  "A record type to hold a genotype and the fitness, if known."
  (genotype)
  (fitness))

;;; OPERATIONS ON POPULATIONS OF INDIVIDUALS

(defun make-population (size min-value max-value min-size max-size)
  "Generate an initial population made from random geotypes.
The result is a list of SIZE lists, each of which is randomly
created with values from the interval [MIN-VALUE, MAX-VALUE), and
sizes from the interval [MIN-SIZE, MAX-SIZE)."
  (loop repeat size collect
        (make-individual :genotype (make-random-genotype min-value
                                                         max-value
                                                         min-size
                                                         max-size))))

(defun evaluate-population (population fitness-function)
  "Evaluate POPULATION assigning a fitnesses using FITNESS-FUNCTION.
Population is a list of individuals ie (program . fitness) pairs.
The result is a new population list, sorted by fitness.
Individuals with NIL for fitness are evaluated.  Individuals that
already have a fitness value are not reassessed; this avoids
unnecessary reevaluation in cases where individuals are copied
into the next generation verbatim."
  (non-destructive-sort
   (mapcar (lambda (individual)
             (if (null (individual-fitness individual))
                 (make-individual :genotype (individual-genotype individual)
                                  :fitness (funcall fitness-function 
                                                    (individual-genotype individual)))
                 individual))
           population)
   (lambda (left right)
     (> (individual-fitness left) (individual-fitness right)))))

(defun tournament (population 
                   crossover-function
                   crossover-probability
                   mutation-function
                   mutation-probability
                   tournament-size)
  "Use tournament selection to select and breed a new generation.
Random pairs of individuals are selected from POPULATION.
CROSSOVER-FUNCTION must take two individual genotypes and return two
children (by multiple value return).  CROSSOVER-PROBABILITY is the
proportion of the resulting population that will be created by
crossover.  MUTATION-FUNCTION should be a function taking one
genotype, and return a mutated genotype.  MUTATION-PROBABILITY is the
proportion of the resulting population that will be created by
mutating existing individuals.  TOURNAMENT-SIZE is the number of
individuals to select at random for each tournament.  The new
population will have the same number of individuals as the given
population, and if there are not enough created by crossover or
mutation, copies of winning individuals will be used to make up the
remainder."
  (let* ((vec (make-array (length population) :initial-contents population))
         (size (length vec))
         (crossovers (quantise (* (- size 1) crossover-probability) 2))
         (mutations (min (- size crossovers 1) 
                         (floor (* size mutation-probability))))
         (copies (max (- size crossovers mutations 1) 0)))
    (labels ((winner ()
               (first
                (sort
                 (loop 
                    repeat tournament-size
                    collect (aref vec (random size)))
                 (lambda (a b)
                   (> (individual-fitness a)
                      (individual-fitness b)))))))
      (append (loop repeat (/ crossovers 2) append
                   (multiple-value-bind (offspring-1 offspring-2)
                       (funcall crossover-function 
                                (individual-genotype (winner))
                                (individual-genotype (winner)))
                     (list (make-individual :genotype offspring-1)
                           (make-individual :genotype offspring-2))))
              (loop repeat mutations collect
                    (make-individual 
                     :genotype (funcall mutation-function 
                                        (individual-genotype (winner)))))
              (loop repeat copies collect (winner))
              (list (first population))))))

;; TODO roulette, roulette-tournament, ... ?
 