;;; For embedded interpreter.
(define (p:sum p1 p2)
  (lambda (succeed fail)
    (p1 (lambda (v1)
          (p2 (lambda (v2) (succeed (+ v1 v2))) fail)) fail)))

(define (p:scale p1 c)
  (lambda (succeed fail)
    (p1 (lambda (v1) (succeed (* v1 c))) fail)))

(define (p:shift p1 c)
  (lambda (succeed fail)
    (p1 (lambda (v1) (succeed (+ v1 c))) fail)))

(define (p:mult p1 p2)
  (lambda (succeed fail)
    (p1 (lambda (v1)
      (p2 (lambda (v2) (succeed (* v1 v2))) fail)) fail)))

(define p:uniform
  (lambda (s f) (s (random 1.0))))

(define (p:pseudonormal d)
  (let ((distr
          (if (= d 0)
              (p:scale (p:shift p:uniform -0.5) (sqrt 12))
              (p:pseudonormal (- d 1)))))
    (p:scale (p:sum distr distr) (/ 1 (sqrt 2)))))

(define p:normal
  (p:pseudonormal 2))

(define (p:display-samples p1 samples)
  (let ((v1 (car (p:value 1 (lambda (s f) (p1 (lambda (v) (s v)) '()))))))
    (p:display-values v1 v1 (p:value samples (lambda (s f) (p1 (lambda (v) (s v)) '()))))))

(define (p:display p1)
  (p:display-samples p1 1000))