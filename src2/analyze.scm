;;;; Analyzing interpreter with AMB.
;;;   Execution procedures take environment
;;;   and two continuations: SUCCEED and FAIL

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define analyze
  (make-generic-operator 1 'analyze
    (lambda (exp)
      (cond ((application? exp) (analyze-application exp))
            (else (error "Unknown expression type" exp))))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp env fail)))

(defhandler analyze analyze-self-evaluating self-evaluating?)


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval env fail))))

(defhandler analyze analyze-quoted quoted?)


(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) env fail)))

(defhandler analyze analyze-variable variable?)

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-compound-procedure vars bproc env)
               env fail))))

(defhandler analyze analyze-lambda lambda?)


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value pred-env pred-fail)
               (if (true? pred-value)
                   (cproc pred-env succeed pred-fail)
                   (aproc pred-env succeed pred-fail)))
             fail))))

(defhandler analyze analyze-if if?)

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1 env
             (lambda (proc1-value proc1-env proc1-fail)
               (proc2 proc1-env succeed proc1-fail))
             fail)))
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
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc proc-env proc-fail)
               (get-args aprocs proc-env
                         (lambda (args args-env args-fail)
                           (execute-application proc
                                                args
                                                args-env
                                                succeed
                                                args-fail))
                         proc-fail))
             fail))))

(define (get-args aprocs env succeed fail)
  (cond ((null? aprocs) (succeed '() env fail))
        ((null? (cdr aprocs))
         ((car aprocs) env
          (lambda (arg new-env fail)
            (succeed (list arg) new-env fail))
          fail))
        (else
         ((car aprocs) env
          (lambda (arg new-env fail)
            (get-args (cdr aprocs) new-env
                      (lambda (args newer-env fail)
                        (succeed (cons arg args)
                                 newer-env fail))
                      fail))
          fail))))

(define execute-application
  (make-generic-operator 5 'execute-application
    (lambda (proc args env succeed fail)
      (error "Unknown procedure type" proc))))

(defhandler execute-application
  (lambda (proc args env succeed fail)
    (succeed (apply-primitive-procedure proc args) env fail))
  strict-primitive-procedure?)

(defhandler execute-application
  (lambda (proc args calling-env succeed fail)
    ((procedure-body proc)
     (extend-environment (procedure-parameters proc)
                         args
                         (procedure-environment proc))
     (lambda (val env fail) (succeed val calling-env fail))
     fail))
  compound-procedure?)

(define (analyze-undoable-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val new-env val-fail)
               (succeed 'OK
                 (extend-environment-one var val new-env)
                  val-fail))
             fail))))

(defhandler analyze
  analyze-undoable-assignment
  assignment?)

;;; TODO set!! permanent assignment?

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val new-env val-fail)
               (succeed var
                 (extend-environment-one var val new-env)
                 val-fail))
             fail))))

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
