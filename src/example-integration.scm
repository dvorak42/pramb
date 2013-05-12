;;; Perform a Monte Carlo integration.
;;;   func is a function on d-tuples
;;;   num is the number of points to sample
;;;   rand randomly samples d-tuples (in some domain)
;;;   area is the area of the domain
;;; This samples num points, takes the average of func over these points, and
;;; multiples the average function value by the domain's area.

(define (monte-carlo-integrate func num rand area)
  (define (sample sum n)
    (if (>= n num)
      (/ sum n)
      (sample (+ sum (func (rand))) (+ n 1))))
  (* area (sample 0 0)))

;;; This function's integral over the triangle domain is 37/24 ~= 1.5416667

(define (sample-func point)
  (let ((x (car point))
        (y (cadr point)))
    (* (+ 1 x) (+ 2 y))))

;;; We define four different functions for sampling points from the triangle
;;; bounded by x=0, y=0, x+y=1.

;;; Generate a point in the unit square, and if it's outside the triangle, use
;;; the point symmetrically opposite the line x+y=1, which must be in the
;;; triangle. This definitely yields a uniform sample.

(define (triangle-symmetry)
  (let ((x (rand-float 0 1))
        (y (rand-float 0 1)))
    (if (<= (+ x y) 1)
        (list x y)
        (list (- 1 y) (- 1 x)))))

#|
;;; Amb-Eval input:
(monte-carlo-integrate sample-func 500 triangle-symmetry 0.5)

;;; Starting a new problem
;;; Amb-Eval value:
1.5396796817819285
|#

;;; Similar to (require), but this doesn't use amb and instead recursively
;;; calls itself to generate a completely new point upon failures. This
;;; definitely yields a uniform sample.

(define (triangle-recursive)
  (let ((x (rand-float 0 1))
        (y (rand-float 0 1)))
    (if (> (+ x y) 1)
        (triangle-recursive)
        (list x y))))

#|
;;; Amb-Eval input:
(monte-carlo-integrate sample-func 500 triangle-recursive 0.5)

;;; Starting a new problem
;;; Amb-Eval value:
1.5398318798201525
|#

;;; Generate an x, then pick a y to guarantee that the point is in the
;;; triangle. This does NOT yield a uniform sample. This gives the same sample
;;; that a DFS would give.

(define (triangle-explicit)
  (let ((x (rand-float 0 1)))
    (let ((y (rand-float 0 (- 1 x))))
      (list x y))))

#|
;;; Amb-Eval input:
(monte-carlo-integrate sample-func 500 triangle-explicit 0.5)

;;; Starting a new problem
;;; Amb-Eval value:
1.6623240089424498
|#

;;; Use a (require) statement. This is the best way in the amb style of
;;; programming, but we're not confident that it produces a uniform sample
;;; due to the BFS scheduler.

(define (triangle-amb)
  (let ((x (amb-range 0 1))
        (y (amb-range 0 1)))
    (require (<= (+ x y) 1))
    (list x y)))

#|
;;; Amb-Eval input:
(monte-carlo-integrate sample-func 500 triangle-amb 0.5)

;;; Starting a new problem
;Aborting!: out of memory
|#
