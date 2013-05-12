;;; genetic-algorithm.lisp --- Toy genetic algorithm library
;; Copyright (c) 2010-2013 Thomas Munro <munro@ip9.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Educational excercises to learn about basic genetic algorithm
;; programming.
;;
;; REFERENCES
;;
;; * http://en.wikipedia.org/wiki/Genetic_algorithm
;; * http://en.wikipedia.org/wiki/Crossover_(genetic_algorithm)

;;; History:
;; 

(in-package :incubarium)

;;; Code:

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
  (let ((min-value min-value)
        (max-value max-value))
    (lambda (genotype)
      (let ((point (random (length genotype))))
        (append (take genotype point)
                (list (+ min-value (random (- max-value min-value))))
                (drop genotype (1+ point)))))))

;; TODO
;; * more kinds of mutation, support for real values etc
;; * element swapping, ...

;; OPERATIONS ON INDIVIDUALS (GENOTYPE/FITNESS PAIRS)

(defun make-individual (genotype &optional fitness)
  "Construct an individual from GENOTYPE and FITNESS."
  ;; we just use pairs to hold genotype/fitness for now
  ;; TODO is there any point in defining a struct for this?
  ;; I like the simplicity of the pair-based approach
  (cons genotype fitness))

(defun individual-genotype (individual)
  "Return the genotype component of INDIVIDUAL."
  (car individual))

(defun individual-fitness (individual)
  "Return the fitness component of INDIVIDUAL."
  (cdr individual))

;;; OPERATIONS ON POPULATIONS OF INDIVIDUALS

(defun make-population (size min-value max-value min-size max-size)
  "Generate an initial population made from random geotypes.
The result is a list of SIZE lists, each of which is randomly
created with values from the interval [MIN-VALUE, MAX-VALUE), and
sizes from the interval [MIN-SIZE, MAX-SIZE)."
  (loop repeat size collect
        (make-individual
         (make-random-genotype min-value max-value min-size max-size))))

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
                 (make-individual 
                  (individual-genotype individual)
                  (funcall fitness-function 
                           (individual-genotype individual)))
                 individual))
           population)
   (lambda (left right)
     (< (individual-fitness left) (individual-fitness right)))))

(defun tournament (population 
                   crossover-function
                   crossover-probability
                   mutation-function
                   mutation-probability)
  "Use tournament selection to select and breed a new generation.
Random pairs of individuals are selected from POPULATION.
CROSSOVER-FUNCTION must take two individual genotypes and return
two children (by multiple value return).  CROSSOVER-PROBABILITY
is the proportion of the resulting population that will be
created by crossover.  MUTATION-FUNCTION should be a function
taking one genotype, and return a mutated genotype.
MUTATION-PROBABILITY is the proportion of the resulting
population that will be created by mutating existing individuals.
The new population will have the same number of individuals as
the given population, and if there are not enough created by
crossover or mutation, copies of winning individuals will be used
to make up the remainder."
  (when (> (+ crossover-probability mutation-probability) 1.0)
    (error "incompatible crossover-probability and mutation-probability"))
  (let* ((vec (make-array (length population) :initial-contents population))
         (size (length vec))
         (crossovers (quantise (* size crossover-probability) 2))
         (mutations (max (- size crossovers) 
                         (floor (* size mutation-probability))))
         (copies (- size crossovers mutations)))
    (labels ((winner ()
               (let ((individual-1 (aref vec (random size)))
                     (individual-2 (aref vec (random size))))
                 (if (> (individual-fitness individual-1)
                        (individual-fitness individual-2))
                     individual-1
                     individual-2))))
      (append (loop repeat crossovers append
                      (multiple-value-bind (a b)
                          (funcall crossover-function 
                                   (individual-genotype (winner))
                                   (individual-genotype (winner)))
                        (list (make-individual a)
                              (make-individual b))))
              (loop repeat mutations collect
                    (make-individual 
                     (funcall mutation-function 
                              (individual-genotype (winner)))))
              (loop repeat copies collect (winner))))))

;; TODO roulette, roulette-tournament, ... ?
