;;; For embedded interpreter.
(define (p:sum p1 p2)
  (lambda (succeed fail)
    (p1 (lambda (v1)
          (p2 (lambda (v2) (succeed (+ v1 v2))) fail)) fail)))

(define (p:mult p1 p2)
  (lambda (succeed fail)
    (p1 (lambda (v1)
          (p2 (lambda (v2) (succeed (* v1 v2))) fail)) fail)))

(define p:uniform
  (lambda (s f) (s (random 1.0))))

(define (p:pseudonormal d)
  (if (= d 0)
      (p:sum p:uniform p:uniform)
      (p:sum (p:pseudonormal (- d 1)) (p:pseudonormal (- d 1)))))

(define p:normal
  (p:pseudonormal 2))
