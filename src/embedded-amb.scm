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
     (succeed (rand-float low high)))))

(define (amb-ranges n low high)
  (define (make-floats k)
    (if (= k 0)
        '()
        (cons (rand-float low high) (make-floats (- k 1)))))
  (ambc
    (lambda (succeed fail)
      (succeed (make-floats n)))))

(define (require p)
  (if (not p) (fail)))

(define (amb-unit-sphere)
  (let ((x (amb-range -1 1))
        (y (amb-range -1 1))
        (z (amb-range -1 1)))
    (require (<= (+ (square x) (square y) (square z)) 1))
    (list x y z)))
