(declare (usual-integrations))


(define (identity x) x)

(define (any? x) #t)


(define ((compose f g) x) (f (g x)))


;;; This is to keep the Scheme printer from going into an infinite
;;; loop if you try to print a circular data structure, such as an
;;; environment

(set! *unparser-list-depth-limit* 10)
(set! *unparser-list-breadth-limit* 10)

;;; Wrappers to make Scheme's random more user-friendly

(define (rand-int low high)
  (+ low (random (- high low))))

;;; Using a modulus of 1.0 is probably safest if we want a uniform distribution

(define (rand-float low high)
  (+ low (* (- high low) (random 1.0))))
