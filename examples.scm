; Let's allow Scheme expressions to have multiple values.
; Infintely many values, even.

; We can provide special forms to average multi-valued expressions or
; display the distribution of values.

(amb-range 0 1)
(amb)
(amb)
(amb)

; Suppose we have a primitive called amb-range
(define amb-unit-sphere
  (let ((x (amb-range -1 1))
	(y (amb-range -1 1))
	(z (amb-range -1 1)))
    (if (< (+ (square x) (square y) (square z)) 1) (amb))
    (list x y z)))

; How many random iterations to average
(amb-set-precision 10)

; Monte Carlo integration of a function f over the unit sphere
(f (amb-unit-sphere))
*ambiguous-distribution*

; The distribution object contains a continuation: we can evaluate it
; on-demand using several different strategies.
(amb-average-with-precision (f (amb-unit-sphere)) 10)

(amb-cont (regular-amb . options)) === (amb . options)

(define (regular-amb . options)
  (lambda (succeed fail)
    (if (null? options)
	(fail)
	(let ((result (car options)))
	  (set! options (cdr options))
	  (succeed result)))))

(define (amb-range low high)
  (lambda (succeed fail)
    (succeed (rand low high))))

(define ((ambify func) . args) (amb (func args)))
(ambify regular-amb) === amb


Multiple evaluation strategies:
stop at the first non-failing branch (current amb's strategy)
aggregate over all non-failing branches (only good for discrete)
aggregate over predetermined number of non-failing branches (monte carlo integration, etc.)

automatically select an evaluation strategy?
