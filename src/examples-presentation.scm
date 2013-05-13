;;; (load "embedded-prob.scm")

; probability visualizations

(p:display-samples p:uniform 10000)

(p:display-samples (p:scale p:uniform 3) 10000)

(p:display-samples p:normal 10000)

(p:display-samples (p:mult p:uniform p:normal) 10000)

; using prob objects

(define (p:samples distr n)
  (p:value n (lambda (s f) (distr (lambda (v) (s v)) '()))))

(define (compute-variance distr n)
  (let ((samples (p:samples distr n)))
    (let ((mu (/ (apply + samples) (length samples))))
      (let ((terms (map (lambda (x) (square (- x mu))) samples)))
        (/ (apply + terms) (length samples))))))

(compute-variance p:normal 1000)

;;; (init "embedded-amb.scm")

; multiple-dwellings

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

; Monte Carlo integration

(define (monte-carlo-integrate func num rand area)
  (define (sample sum n)
    (if (>= n num)
      (/ sum n)
      (sample (+ sum (func (rand))) (+ n 1))))
  (* area (sample 0 0)))

(define (triangle-amb)
  (let ((point (amb-ranges 2 0 1)))
    (require (<= (+ (car point) (cadr point)) 1))
    point))

(define (example-polynomial point)
  (let ((x (car point))
        (y (cadr point)))
    (* (+ 1 x) (+ 2 y))))

(monte-carlo-integrate example-polynomial 20 triangle-amb 0.5)

; demonstrating undo-able set!

(define p #f)

(let ((x (amb 1 2 3)))
  (if (= x 2) (set! p #t))
  (let ((y (amb 1 2)))
    (list x y p)))

; factoring (from slides)

(define (amb-count-up start)
  (ambc
    (lambda (succeed fail)
      (let ((result start))
        (set! start (+ start 1))
        (succeed result)))))

(define (factor num)
  (let ((a (amb-count-up 1))
        (b (amb-count-up 1)))
    (if (not (= (* a b) num)) (fail))
    (list a b)))

; demonstrate BFS scheduling

(define (demo-bfs)
  (let ((a (amb-count-up 1))
        (b (amb-count-up 1))
        (c (amb-count-up 1)))
    (list a b c)))
