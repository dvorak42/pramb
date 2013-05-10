;;; This file should be run in the embedded interpreter.

(define (amb-range low high)
  (ambc
   (lambda (succeed fail)
     (succeed (+ low (random (exact->inexact (- high low))))))))

(define (amb . options)
  (ambc (lambda (s f)
	  (if (null? options)
	      (f)
	      (let ((result (car options)))
		(set! options (cdr options))
		(s result))))))
