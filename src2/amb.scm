;;; amb iterates through a list of alternatives (in order), then fails

(define (amb? exp)
  (and (pair? exp) (eq? (car exp) 'amb)))

(define (amb-alternatives exp) (cdr exp))

(define (analyze-amb exp)
  (let ((aprocs (map analyze (amb-alternatives exp))))
    (lambda (env succeed fail)
      (let loop ((alts aprocs))
        (if (null? alts)
            (fail)
            ((car alts) env
                        succeed
                        (lambda ()
                          (loop (cdr alts)))))))))

(defhandler analyze analyze-amb amb?)

;;; amb-range provides a new real number in a given range, and never fails

(define (amb-range? exp)
  (and (pair? exp) (eq? (car exp) 'amb-range)))

(define (amb-range-low exp) (cadr exp))
(define (amb-range-high exp) (caddr exp))

(define (analyze-amb-range exp)
  (let ((low (analyze (amb-range-low exp)))
        (high (analyze (amb-range-high exp))))
    (lambda (env succeed fail)
      (low env
        (lambda (low-val low-env low-fail)
          (high low-env
            (lambda (high-val high-env high-fail)
              (let loop ()
                ((analyze (rand-range low-val high-val))
                  high-env
                  succeed
                  loop)))
            low-fail))
        fail))))

(defhandler analyze analyze-amb-range amb-range?)
