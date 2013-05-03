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
    (if (> (+ (square x) (square y) (square z)) 1) (amb))
    (list x y z)))

; How many random iterations to average
(amb-set-precision 10)

; Monte Carlo integration of a function f over the unit sphere
(f (amb-unit-sphere))
*ambiguous-distribution*

; The distribution object contains a continuation: we can evaluate it
; on-demand using several different strategies.
(amb-average-with-precision (f (amb-unit-sphere)) 10)

