;;;; Analyzing interpreter with AMB support (in amb.scm).
;;;   Execution procedures take a SUCCEED continuation.

(define *env*)

(define analyze
  (make-generic-operator 1 'analyze
    (lambda (exp)
      (cond ((application? exp) (analyze-application exp))
            (else (error "Unknown expression type" exp))))))

(define (analyze-self-evaluating exp)
  (lambda (succeed) (succeed exp)))

(defhandler analyze analyze-self-evaluating self-evaluating?)


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (succeed) (succeed qval))))

(defhandler analyze analyze-quoted quoted?)


(define (analyze-variable exp)
  (lambda (succeed) (succeed (lookup-variable-value exp *env*))))

(defhandler analyze analyze-variable variable?)


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    (lambda (succeed)
      (succeed (make-compound-procedure vars bproc *env*)))))

(defhandler analyze analyze-lambda lambda?)


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (succeed)
      (pproc (lambda (pred-value)
               (if (true? pred-value)
                   (cproc succeed)
                   (aproc succeed)))))))

(defhandler analyze analyze-if if?)


(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (succeed)
      (proc1 (lambda (proc1-value)
               (proc2 succeed)))))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (if (null? exps) (error "Empty sequence"))
  (let ((procs (map analyze exps)))
    (loop (car procs) (cdr procs))))

(defhandler analyze
  (lambda (exp)
    (analyze-sequence (begin-actions exp)))
  begin?)


(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (succeed)
      (fproc (lambda (proc)
               (get-args aprocs
                         (lambda (args)
                           (execute-application proc
                                                args
                                                succeed))))))))

(define (get-args aprocs succeed)
  (cond ((null? aprocs) (succeed '()))
        ((null? (cdr aprocs))
         ((car aprocs) (lambda (arg) (succeed (list arg)))))
        (else
         ((car aprocs)
          (lambda (arg)
            (get-args (cdr aprocs)
                      (lambda (args)
                        (succeed (cons arg args)))))))))

(define execute-application
  (make-generic-operator 3 'execute-application
    (lambda (proc args succeed)
      (error "Unknown procedure type" proc))))

(defhandler execute-application
  (lambda (proc args succeed)
    (succeed (apply-primitive-procedure proc args)))
  strict-primitive-procedure?)

(defhandler execute-application
  (lambda (proc args succeed)
    (let ((func (if (list? (procedure-parameters proc)) identity list)))
      (let ((old-env *env*))
	(set! *env* (extend-environment (func (procedure-parameters proc))
					(func args)
					(procedure-environment proc)))
    	((procedure-body proc)
	 (lambda (val)
	   (set! *env* old-env)
	   (succeed val))))))
  compound-procedure?)


(define (analyze-undoable-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (succeed)
      (vproc (lambda (val)
               (set-variable-value! var val *env*)
               (succeed 'OK))))))

(defhandler analyze
  analyze-undoable-assignment
  assignment?)


(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (succeed)
      (vproc (lambda (val)
	       (define-variable! var val *env*)
               (succeed val))))))

(defhandler analyze analyze-definition definition?)


;;; Macros (definitions are in syntax.scm)

(defhandler analyze
  (lambda (exp)
    (analyze (cond->if exp)))
  cond?)

(defhandler analyze
  (lambda (exp)
    (analyze (let->combination exp)))
  let?)
