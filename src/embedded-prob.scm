;;; For embedded interpreter.
(define (p:sum p1 p2)
  (lambda (succeed fail)
    (p1 (lambda (v1)
          (p2 (lambda (v2) (succeed (+ v1 v2))) fail)) fail)))

(define (p:mult p1 p2)
  (lambda (succeed fail)
    (p1 (lambda (v1)
          (p2 (lambda (v2) (succeed (* v1 v2))) fail)) fail)))

