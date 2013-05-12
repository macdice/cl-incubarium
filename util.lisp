(in-package :incubarium)

(defun quantise (x step)
  "Round X down to the nearest multiple of STEP."
  (* (floor (/ x step)) step))

(defun take (list n)
  "Take from LIST only the first N elements.  From SRFI-1."
  (if (zerop n) nil (cons (car list) (take (cdr list) (- n 1)))))

(defun drop (list n)
  "Take from LIST the tail after the first N elements.  From SRFI-1."
  (if (zerop n) list (drop (cdr list) (- n 1))))

(defun filter (predicate list)
  (loop 
     for object in list
     if (funcall predicate object)
     collect object))

(defun non-destructive-sort (list predicate)
  "Return a new list with the elements of LIST sorted using PREDICATE."
  ;; this function is here because I hate the fact that Emacs Lisp's
  ;; SORT (just like that of Common Lisp, MacLisp and probably earlier
  ;; dialects) is destructive -- this seems to me to be wanton
  ;; destruction up with which I will not put; there should have been
  ;; a SORT and an NSORT (cf SRFI-95 SORT and SORT!, although the Nxxx
  ;; functions don't promise to mutate their input to achieve their
  ;; goal like the xxx! functions, they merely have the option of
  ;; doing so... an interesting difference), and if I ever succeed in
  ;; building a time machine (easy) and getting into MIT (hard) I will
  ;; make my opinion known at the appropriate time to sort this issue
  ;; out once and for all
  (let ((temporary (copy-seq list)))
    (sort temporary predicate)))

(defun pick-one (list)
  "Return a randomly selected item from LIST."
  (elt list (random (length list))))
