(define *fail-queue*)   ; this is set in driver-loop in repl.scm
(define *global-fail*)  ; this is set in driver-loop in repl.scm

(define (add-branch cont)
  (enqueue! *fail-queue* cont))
(define (fail)
  (if (queue-empty? *fail-queue*)
      (*global-fail*)
      ((dequeue! *fail-queue*))))

;;; amb iterates through a list of alternatives (in order), then fails

(define (amb? exp)
  (and (pair? exp) (eq? (car exp) 'amb)))

(define (amb-alternatives exp) (cdr exp))

(define (analyze-amb exp)
  (let ((aprocs (map analyze (amb-alternatives exp))))
    (lambda (env succeed dummy-fail)
      (let loop ((alts aprocs))
        (if (null? alts)
            (fail)
	    (begin
	      (add-branch (lambda () (loop (cdr alts))))
	      ((car alts) env succeed 'dummy-fail)))))))

(defhandler analyze analyze-amb amb?)

;;; ambc is called with a continuation that is passed success and
;;; failure continuations which it calls at its discretion.

(define (ambc? exp)
  (and (pair? exp) (eq? (car exp) 'ambc)))

(define (analyze-ambc exp)
  (let ((fproc (analyze (cadr exp))))
    (lambda (env succeed dummy-fail)
      (fproc env
	     (lambda (proc proc-env dummy-fail) 
	       (let loop ()
		 (add-branch loop)
		 (execute-application
		  proc
		  (list (lambda (r)
			  (succeed r proc-env 'dummy-fail))
			fail)
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
    (lambda (env succeed dummy-fail)
      (low env
        (lambda (low-val low-env dummy-fail)
          (high low-env
            (lambda (high-val high-env dummy-fail)
              (let loop ()
		(add-branch loop)
                ((analyze (rand-range low-val high-val))
                  high-env
                  succeed
                  loop)))
            'dummy-fail))
        'dummy-fail))))

(defhandler analyze analyze-amb-range amb-range?)
