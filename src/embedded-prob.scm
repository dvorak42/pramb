;;; For embedded interpreter.

;;; Returns the probobj representing the sum of two probobjs.
(define (p:sum p1 p2)
  (lambda (succeed fail)
    (p1 (lambda (v1)
          (p2 (lambda (v2) (succeed (+ v1 v2))) fail)) fail)))

;;; Returns the probobj representing p1 scaled by a factor of c.
(define (p:scale p1 c)
  (lambda (succeed fail)
    (p1 (lambda (v1) (succeed (* v1 c))) fail)))

;;; Returns the probobj representing p1 shifted by a factor of c.
(define (p:shift p1 c)
  (lambda (succeed fail)
    (p1 (lambda (v1) (succeed (+ v1 c))) fail)))

;;; Returns the probobj representing the product of two distributions.
(define (p:mult p1 p2)
  (lambda (succeed fail)
    (p1 (lambda (v1)
      (p2 (lambda (v2) (succeed (* v1 v2))) fail)) fail)))

;;; Returns a uniform probobj between 0 and 1.
(define p:uniform
  (lambda (s f) (s (random 1.0))))

;;; Returns a psuedo-normal distribution with depth d using the Central Limit Theorem.
(define (p:pseudonormal d)
  (let ((distr
          (if (= d 0)
              (p:scale (p:shift p:uniform -0.5) (sqrt 12))
              (p:pseudonormal (- d 1)))))
    (p:scale (p:sum distr distr) (/ 1 (sqrt 2)))))

;;; A second level pseudo-normal distribution.
(define p:normal
  (p:pseudonormal 2))

;;; Displays a number of values grabbed from the distribution.
(define (p:display-samples p1 samples)
  (let ((v1 (car (p:value 1 (lambda (s f) (p1 (lambda (v) (s v)) '()))))))
    (p:display-values v1 v1 (p:value samples (lambda (s f) (p1 (lambda (v) (s v)) '()))))))

(define (p:display p1)
  (p:display-samples p1 1000))