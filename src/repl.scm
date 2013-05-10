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

(define input-prompt ";;; Amb-Eval input:\n")

(define output-prompt "\n;;; Amb-Eval value:\n")

(define (init)
  (set! *env-stack*
	(list (extend-environment
	       '() '() the-empty-environment)))
  (set! *proc-envs* '())
  (driver-loop))

(define (driver-loop)
  (let ((input (prompt-for-command-expression input-prompt)))
    (if (eq? input 'try-again) (fail))  ; fail is defined in amb.scm
    (newline)
    (display ";;; Starting a new problem ")
    (set! *fail-queue* (make-queue))
    (set! *global-fail*
	  (lambda ()
	    (display ";;; There are no more values of ")
	    (pp input)
	    (driver-loop)))
    ((analyze input)
     (lambda (val)
       (display output-prompt)
       (pp val)
       (driver-loop)))))
