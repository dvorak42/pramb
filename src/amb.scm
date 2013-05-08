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
  (and (pair? exp) (eq? (car exp) 'amb-orig)))

(define (amb-alternatives exp) (cdr exp))

(define (analyze-amb exp)
  (let ((aprocs (map analyze (amb-alternatives exp))))
    (lambda (env succeed)
      (let loop ((alts aprocs))
        (if (null? alts)
            (fail)
	    (begin
	      (add-branch (lambda () (loop (cdr alts))))
	      ((car alts) env succeed)))))))

(defhandler analyze analyze-amb amb?)

;;; ambc is called with a continuation that is passed success and
;;; failure continuations which it calls at its discretion:
;;;   (ambc (lambda (succeed fail) ...))

(define (ambc? exp)
  (and (pair? exp) (eq? (car exp) 'ambc)))

(define (analyze-ambc exp)
  (let ((fproc (analyze (cadr exp))))
    (lambda (env succeed)
      (fproc env
	     (lambda (proc proc-env) 
	       (let loop ()
		 (execute-application
		  proc
		  (list (lambda (r)
			  (add-branch loop)
			  (succeed r proc-env))
			fail)
		  proc-env
		  (lambda (val env)
		    (error "ambc argument unexpectedly returned")))))))))

(defhandler analyze analyze-ambc ambc?)
