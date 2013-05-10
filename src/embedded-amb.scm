;;; This file should be run in the embedded interpreter.

(define (fail)
  (ambc
   (lambda (succeed fail)
     (fail))))

(define (amb . options)
  (ambc
   (lambda (succeed fail)
     (if (null? options)
	 (fail)
	 (let ((result (car options)))
	   (set! options (cdr options))
	   (succeed result))))))

(define (amb-range low high)
  (ambc
   (lambda (succeed fail)
     (succeed (+ low (random (exact->inexact (- high low))))))))

(define (amb-unit-sphere)
  (let ((x (amb-range -1 1))
	(y (amb-range -1 1))
	(z (amb-range -1 1)))
    (if (> (+ (square x) (square y) (square z)) 1) (fail))
    (list x y z)))

