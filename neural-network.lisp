;;; INCUBARIUM
;; (c) 2010-2013 Thomas Munro <munro@ip9.org>

;; This is loosely based on/ported from public domain Python code by
;; Neil Schemenauer[1].  The Lisp code is probably not as readable as
;; the Python original, I'm afraid, mainly down to the lack of concise
;; array subscript syntax, but I did try to make use of the mighty
;; LOOP's ability to walk data structures in parallel rather that
;; using AREF to explicitly get values from vectors, wherever possible
;; (ie when reading in the appropriate order).
;;
;; The code uses Scheme-style exclamation marks to indicate mutation
;; of function arguments, because today I am feeling schematic.  
;;
;; Some relevant web pages:
;; 
;; * http://en.wikipedia.org/wiki/Backpropagation
;; * http://www.tek271.com/?about=docs/neuralNet/IntoToNeuralNets.html
;; * http://galaxy.agh.edu.pl/~vlsi/AI/backp_t_en/backprop.html
;; * http://arctrix.com/nas/python/bpnn.py [1]
;; * http://en.wikipedia.org/wiki/Logistic_function

(in-package :incubarium)

;;; NUMERIC UTILITIES

(defun random-number-in-range (min max)
  "Return a random floating point number in the range [MIN, MAX)."
  (+ min (random (- max min))))

(defun logistic (x)
  "The logistic function, mapping X to the range 0, 1 (a sigmoid function)."
  (/ 1 (+ 1 (exp (- x)))))

(defun logistic-derivative (x)
  "The derivative of the logistic function, mapping X to the range 0, 1."
  (* (logistic x) (- 1 (logistic x))))

(defun tanh-derivative (y)
  "The derivative of TANH in terms of Y."
  (- 1.0 (* y y)))

;;; VECTORS AND MATRICES

(defun make-matrix (rows columns)
  "Construct a new matrix."
  (make-array (list rows columns) 
              :element-type 'double-float 
              :initial-element 0.0d0))

(defun make-vector (elements &optional (fill 0.0))
  "Construct a new vector."
  (make-array elements
              :element-type 'double-float
              :initial-element fill))

;;; OPERATIONS ON NUERAL NETWORKS

(defstruct neural-network
  "A neural network."
  (input-count)
  (hidden-count)
  (output-count)
  (input-activation)
  (hidden-activation)
  (output-activation)
  (input-weights)
  (output-weights)
  (input-change)
  (output-change))

(defun create-neural-network (input-count hidden-count output-count)
  "Return a new artificial neural network.
The input layer will have INPUT-COUNT input nodes, HIDDEN-COUNT
nodes in the hidden layer, and OUTPUT-COUNT output nodes.  The
weights are randomly initialised."
  (let* ((input-weights (make-matrix (1+ input-count) hidden-count))
         (output-weights (make-matrix hidden-count output-count))
         (result (make-neural-network
                  :input-count (1+ input-count)
                  :hidden-count hidden-count
                  :output-count output-count
                  :input-activation (make-vector (1+ input-count) 1.0d0)
                  :hidden-activation (make-vector hidden-count 1.0d0)
                  :output-activation (make-vector output-count 1.0d0)
                  :input-weights input-weights
                  :output-weights output-weights
                  :input-change (make-matrix (1+ input-count) hidden-count)
                  :output-change (make-matrix hidden-count output-count))))
    ;; initialise weights to random values
    (loop for i from 0 below input-count do
          (loop for j from 0 below hidden-count do
                (setf (aref input-weights i j) (random-number-in-range -0.2d0 0.2d0))))
    (loop for j from 0 below hidden-count do
          (loop for k from 0 below output-count do
                (setf (aref output-weights j k) (random-number-in-range -2.0d0 2.0d0))))
    result))

(defun update-neural-network! (network values)
  "Update the input activation nodes of NETWORK with VALUES.
The the output activation node vector is returned."
  (unless (= (length values) (1- (neural-network-input-count network)))
    (error "Wrong number of inputs"))
  ;; set input activations
  (loop for value across values
        for i from 0 do
        (setf (aref (neural-network-input-activation network) i) value))
  ;; compute hidden activations
  (let ((input-weights (neural-network-input-weights network))
        (input-activation (neural-network-input-activation network))
        (hidden-activation (neural-network-hidden-activation network)))
    (loop for j from 0 below (neural-network-hidden-count network) do
          (setf (aref hidden-activation j)
                (tanh
                 (loop for input-act across input-activation
                       for i from 0
                       sum (* input-act
                              (aref input-weights i j)))))))
  ;; compute output activations
  (let ((output-weights (neural-network-output-weights network))
        (hidden-activation (neural-network-hidden-activation network))
        (output-activation (neural-network-output-activation network)))
    (loop for k from 0 below (neural-network-output-count network) do
          (setf (aref output-activation k)
                (tanh
                 (loop for j from 0 below (neural-network-hidden-count network)
                       sum (* (aref hidden-activation j)
                              (aref output-weights j k)))))))
  (neural-network-output-activation network))

(defun back-propagate! (network targets n m)
  "Adjust the weights of NETWORK to reduce error for expected result TARGETS.
The learning rate should be provided in N, and the momentum in M.
The error is returned."
  (unless (= (length targets) (neural-network-output-count network))
    (error "Wrong number of target values"))
  (let ((output-activation (neural-network-output-activation network))
        (output-weights (neural-network-output-weights network))
        (output-change (neural-network-output-change network))
        (hidden-activation (neural-network-hidden-activation network))
        (input-weights (neural-network-input-weights network))
        (input-change (neural-network-input-change network))
        (input-activation (neural-network-input-activation network)))
    ;; compute the output and hidden errors terms
    (let* ((output-deltas 
            (loop for target across targets
                  for output-act across output-activation
                  as error = (- target output-act)
                  collect (* (tanh-derivative output-act) error)))
           (hidden-deltas 
            (loop for i from 0 ;across output-weights
                  for hidden-act across hidden-activation
                  as error = (loop for output-delta in output-deltas
                                   for j from 0
                                   as weight = (aref output-weights i j)
                                   sum (* output-delta weight))
                  collect (* (tanh-derivative hidden-act) error))))
      ;; update output weights
      (loop for i from 0
            for hidden-act across hidden-activation do
            (loop for k from 0
                  for output-delta in output-deltas
                  as change = (* output-delta hidden-act) do
                  (incf (aref output-weights i k)
                        (+ (* n change)
                           (* m (aref output-change i k))))
                  (setf (aref output-change i k) change)))
      ;; update the input weights
      (loop for i from 0 below (neural-network-input-count network) do
            (loop for j from 0 below (neural-network-hidden-count network)
                  for delta in hidden-deltas
                  for input-act across input-activation
                  as change = (* delta (aref input-activation i)) do
                  (incf (aref input-weights i j)
                        (+ (* n change) (aref input-change i j)))
                  (setf (aref input-change i j) change))))
    ;; calculate and return error
    (loop for target across targets
          for output across output-activation
          sum (* 0.5 (expt (- target output) 2)))))

(defun train-neural-network! (network training-data
                              &optional
                              (iterations 1000)
                              (n 0.5)
                              (m 0.1))
  "Train NETWORK using TRAINING-DATA.
The training data should be a list of lists containing two
vectors of numbers, representing the input values and expected
output values.  The neural network is trained ITERATIONS times,
using learning rate N and momentum M."
  ;; TODO return the error!
  (loop repeat iterations do
        (loop for (inputs targets) in training-data
              as outputs = (update-neural-network! network inputs)
              sum (back-propagate! network targets n m))))
