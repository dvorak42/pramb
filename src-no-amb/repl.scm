; Read-eval-print loop for extended Scheme interpreter
;;;

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

(define (init)
  (let ((initial-env (extend-environment-list '() '() the-empty-environment)))
    (let loop ((env initial-env))
      (let* ((input (read))
             (e (eval input env)))
        (write-line (eval-val e))
        (loop (eval-env e))))))

