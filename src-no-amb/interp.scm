
;;; Core of extended Scheme interpreter
;;;
;;; See also structure and predicate definitions in rtdata.scm and
;;; syntax.scm

(declare (usual-integrations eval apply))

(define (default-eval expression environment)
  (cond ((application? expression)
         (let ((e (eval (operator expression)
                        environment)))
           (apply (eval-val e)
                  (operands expression)
                  (eval-env e))))
        (else
         (error "Unknown expression type" expression))))

(define (default-apply procedure operands calling-environment)
  (error "Unknown procedure type" procedure))



(define eval
  (make-generic-operator 2 'eval default-eval))

(defhandler eval
  (lambda (expression environment) (make-eval expression environment))
  self-evaluating?)

(defhandler eval
  (lambda (expression environment)
    (make-eval (lookup-variable-value expression environment) environment))
  variable?)

(defhandler eval
  (lambda (expression environment)
    (make-eval (text-of-quotation expression) environment))
  quoted?)

(defhandler eval
  (lambda (expression environment)
    (make-eval (make-compound-procedure
                (lambda-parameters expression)
                (lambda-body expression)
                environment)
               environment))
  lambda?)

(defhandler eval
  (lambda (expression environment)
    (let ((e (eval (if-predicate expression) environment)))
      (if (eval-val e)
          (eval (if-consequent expression) (eval-env e))
          (eval (if-alternative expression) (eval-env e)))))
  if?)

(defhandler eval
  (lambda (expression environment)
    (eval (cond->if expression) environment))
  cond?)

(defhandler eval
  (lambda (expression environment)
    (eval (let->combination expression) environment))
  let?)

(defhandler eval
  (lambda (expression environment)
    (evaluate-sequence (begin-actions expression)
                       environment))
  begin?)

(define (evaluate-sequence actions environment)
  (cond ((null? actions)
         (error "Empty sequence"))
        ((null? (rest-exps actions))
         (eval (first-exp actions) environment))
        (else
         (let ((e (eval (first-exp actions) environment)))
           (evaluate-sequence (rest-exps actions) (eval-env e))))))

(defhandler eval
  (lambda (expression environment)
    (let ((e (eval (definition-value expression) environment)))
      (let ((new-env (extend-environment-one
                      (definition-variable expression)
                      (eval-val e)
                      (eval-env e))))
        (make-eval (definition-variable expression) new-env))))
  definition?)

(defhandler eval
  (lambda (expression environment)
    (let ((e (eval (assignment-value expression) environment)))
      (let ((new-env (extend-environment-one
                      (assignment-variable expression)
                      (eval-val e)
                      (eval-env e))))
        (make-eval (eval-val e) new-env))))
  assignment?)


(define apply
  (make-generic-operator 3 'apply default-apply))

(defhandler apply
  (lambda (procedure operands calling-environment)
    (define (evaluate-list operands)
      (cond ((null? operands) '())
            ((null? (rest-operands operands))
             (list (eval-val (eval (first-operand operands)
                                   calling-environment))))
            (else
             (cons (eval-val (eval (first-operand operands)
                                   calling-environment))
                   (evaluate-list (rest-operands operands))))))
    (make-eval
     (apply-primitive-procedure
      procedure
      (evaluate-list operands))
     calling-environment))
  strict-primitive-procedure?)

(defhandler apply
  (lambda (procedure operands calling-environment)
    (if (not (= (length (procedure-parameters procedure))
                (length operands)))
        (error "Wrong number of operands supplied"))
    (let ((arguments
           (map (lambda (parameter operand)
                  (evaluate-procedure-operand parameter
                                              operand
                                              calling-environment))
                (procedure-parameters procedure)
                operands)))
      (let ((e (eval (procedure-body procedure)
                     ;; MODIFIES ENVIRONMENT
                     (extend-environment-list
                      (map procedure-parameter-name
                           (procedure-parameters procedure))
                      arguments
                      (procedure-environment procedure)))))
        (make-eval (eval-val e) calling-environment))))
  compound-procedure?)

(define evaluate-procedure-operand
  (make-generic-operator 3
                         'evaluate-operand
                         (lambda (parameter operand environment)
                           (eval-val (eval operand environment)))))

(define procedure-parameter-name
  (make-generic-operator 1 'parameter-name (lambda (x) x)))
