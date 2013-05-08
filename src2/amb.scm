; TODO queue needs to empty on each (init)
(define fail-queue (make-queue))

;;; amb iterates through a list of alternatives (in order), then fails

(define (amb? exp)
  (and (pair? exp) (eq? (car exp) 'amb)))

(define (amb-alternatives exp) (cdr exp))

(define (analyze-amb exp)
  (let ((aprocs (map analyze (amb-alternatives exp))))
    (lambda (env succeed fail)
      (let loop ((alts aprocs))
        (if (null? alts)
            ((dequeue! fail-queue))
	    (begin
	      (enqueue! fail-queue (lambda () (loop (cdr alts))))
	      ((car alts) env succeed 'dummy-fail)))))))

(defhandler analyze analyze-amb amb?)

;;; ambc is called with a continuation that is passed success and
;;; failure continuations which it calls at its discretion.

(define (ambc? exp)
  (and (pair? exp) (eq? (car exp) 'ambc)))

(define (analyze-ambc exp)
  (let ((fproc (analyze (cadr exp))))
    (lambda (env succeed fail)
      (fproc env
	     (lambda (proc proc-env proc-fail) 
	       (let loop ()
		 (enqueue! fail-queue loop)
		 (execute-application
		  proc
		  (list (lambda (r)
			  (succeed r proc-env 'dummy-fail))
			(lambda () ((dequeue! fail-queue))))
		  proc-env
		  succeed
		  'dummy-fail)))
	     'dummy-fail))))

(defhandler analyze analyze-ambc ambc?)

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
		(enqueue! fail-queue loop)
                ((analyze (rand-range low-val high-val))
                  high-env
                  succeed
                  loop)))
            'dummy-fail))
        'dummy-fail))))

(defhandler analyze analyze-amb-range amb-range?)
