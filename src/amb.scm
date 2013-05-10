(define *fail-queue*)   ; this is set in driver-loop in repl.scm
(define *global-fail*)  ; this is set in driver-loop in repl.scm

(define (add-branch cont)
  (enqueue! *fail-queue* (cons cont (grab-environment-state))))
(define (fail)
  (if (queue-empty? *fail-queue*)
      (*global-fail*)
      (let ((pair (dequeue! *fail-queue*)))
	(restore-environment-state (cdr pair))
	((car pair)))))

;;; amb iterates through a list of alternatives (in order), then fails.
;;; this version is called amb-orig: we define amb in the interpreter
;;; using ambc.

(define (amb? exp)
  (and (pair? exp) (eq? (car exp) 'amb-orig)))

(define (amb-alternatives exp) (cdr exp))

(define (analyze-amb exp)
  (let ((aprocs (map analyze (amb-alternatives exp))))
    (lambda (succeed)
      (let loop ((alts aprocs))
        (if (null? alts)
            (fail)
	    (begin
	      (add-branch (lambda () (loop (cdr alts))))
	      ((car alts) succeed)))))))

(defhandler analyze analyze-amb amb?)

;;; ambc is called with a continuation that is passed success and
;;; failure continuations which it calls at its discretion:
;;;   (ambc (lambda (succeed fail) ...))

(define (ambc? exp)
  (and (pair? exp) (eq? (car exp) 'ambc)))

(define (analyze-ambc exp)
  (let ((fproc (analyze (cadr exp))))
    (lambda (succeed)
      (fproc (lambda (proc)
	       (let loop ()	
		 (execute-application
		  proc
		  (list (lambda (r)
			  (add-branch loop)
			  (succeed r))
			fail)
		  (lambda (val)
		    (error "ambc argument unexpectedly returned")))))))))

(defhandler analyze analyze-ambc ambc?)
