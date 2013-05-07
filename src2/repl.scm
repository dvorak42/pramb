(declare (usual-integrations write write-line pp eval))

(define write
  (make-generic-operator 1 'write
    (access write user-initial-environment)))

(define write-line
  (make-generic-operator 1 'write-line
    (access write-line user-initial-environment)))

(define pp
  (make-generic-operator 1 'pretty-print
    (access pp user-initial-environment)))

(define (procedure-printable-representation procedure)
  `(compound-procedure
    ,(procedure-parameters procedure)
    ,(procedure-body procedure)
    <procedure-environment>))

(defhandler write
  (compose write procedure-printable-representation)
  compound-procedure?)

(defhandler write-line
  (compose write-line procedure-printable-representation)
  compound-procedure?)

(defhandler pp
  (compose pp procedure-printable-representation)
  compound-procedure?)


(define (read) (prompt-for-command-expression "eval> "))


;;; Initialization and driver loop

(define (evaluator exp env succeed fail)
  ((analyze exp)
   env
   succeed
   fail))

(define input-prompt ";;; Amb-Eval input:\n")

(define output-prompt "\n;;; Amb-Eval value:\n")

(define (init)
  (driver-loop (extend-environment '() '() the-empty-environment)))

(define (driver-loop env)
  (define (internal-loop env try-again)
    (let ((input
           (prompt-for-command-expression input-prompt)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (evaluator
             input
             env
             (lambda (val new-env next-alternative) ;;; TODO
               (display output-prompt)
               (pp val)
               (internal-loop new-env next-alternative))
             (lambda ()
               (display ";;; There are no more values of ")
               (pp input)
               (driver-loop env)))))))
  (internal-loop
    env
   (lambda ()
     (display ";;; There is no current problem")
     (driver-loop env))))
