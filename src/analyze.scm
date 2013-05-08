;;;; Analyzing interpreter with AMB support (in amb.scm).
;;;   Execution procedures take environment
;;;   and a SUCCEED continuation.

(define analyze
  (make-generic-operator 1 'analyze
    (lambda (exp)
      (cond ((application? exp) (analyze-application exp))
            (else (error "Unknown expression type" exp))))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed) (succeed exp env)))

(defhandler analyze analyze-self-evaluating self-evaluating?)


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed) (succeed qval env))))

(defhandler analyze analyze-quoted quoted?)


(define (analyze-variable exp)
  (lambda (env succeed) (succeed (lookup-variable-value exp env) env)))

(defhandler analyze analyze-variable variable?)


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    (lambda (env succeed)
      (succeed (make-compound-procedure vars bproc env) env))))

(defhandler analyze analyze-lambda lambda?)


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed)
      (pproc env
             (lambda (pred-value pred-env)
               (if (true? pred-value)
                   (cproc pred-env succeed)
                   (aproc pred-env succeed)))))))

(defhandler analyze analyze-if if?)


(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed)
      (proc1 env
             (lambda (proc1-value proc1-env)
               (proc2 proc1-env succeed)))))
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
    (lambda (env succeed)
      (fproc env
             (lambda (proc proc-env)
               (get-args aprocs proc-env
                         (lambda (args args-env)
                           (execute-application proc
                                                args
                                                args-env
                                                succeed))))))))

(define (get-args aprocs env succeed)
  (cond ((null? aprocs) (succeed '() env))
        ((null? (cdr aprocs))
         ((car aprocs) env
          (lambda (arg new-env)
            (succeed (list arg) new-env))))
        (else
         ((car aprocs) env
          (lambda (arg new-env)
            (get-args (cdr aprocs) new-env
                      (lambda (args newer-env)
                        (succeed (cons arg args)
                                 newer-env))))))))

(define execute-application
  (make-generic-operator 4 'execute-application
    (lambda (proc args env succeed)
      (error "Unknown procedure type" proc))))

(defhandler execute-application
  (lambda (proc args env succeed)
    (succeed (apply-primitive-procedure proc args) env))
  strict-primitive-procedure?)

(defhandler execute-application
  (lambda (proc args env succeed)
    (let ((func (if (list? (procedure-parameters proc)) identity list)))
      ((procedure-body proc)
       (extend-environment (func (procedure-parameters proc))
			   (func args)
			   (procedure-environment proc))
       (lambda (val new-env) (succeed val env)))))  ; *not* new-env
  compound-procedure?)

(define (analyze-undoable-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed)
      (vproc env
             (lambda (val new-env)
	       (set-variable-value! var val new-env)
               (succeed 'OK new-env))))))

(defhandler analyze
  analyze-undoable-assignment
  assignment?)

;;; TODO set!! permanent assignment?

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed)
      (vproc env
             (lambda (val new-env)
               (succeed var
                 (extend-environment-one var val new-env)))))))

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
