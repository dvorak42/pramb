(declare (usual-integrations))

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
		 (define stack (grab-env-stack))
		 (execute-application
		  proc
		  (list (lambda (r)
			  (set-env-stack! stack)
			  (add-branch loop)
			  (succeed r))
			fail)
		  (lambda (val)
		    (error "ambc argument unexpectedly returned")))))))))

(defhandler analyze analyze-ambc ambc?)
