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


;;; Initialization and driver loop

(define input-prompt ";;; Amb-Eval input:\n")

(define output-prompt "\n;;; Amb-Eval value:\n")

(define (init . files)
  (reset-environment-state)
  (map (lambda (file)
         (call-with-input-file file driver-loop))
       files)
  (driver-loop))

(define (driver-loop #!optional port)
  (let ((input
         (if (default-object? port)
             (prompt-for-command-expression input-prompt)
             (let ((file-line (read port)))
               (display input-prompt)
               (pp file-line)
               file-line))))
    (if (not (eof-object? input))
        (begin
          (if (eq? input 'try-again) (fail))  ; fail is defined in amb.scm
          (newline)
          (display ";;; Starting a new problem ")
          (set! *fail-queue* (make-queue))
          (set! *global-fail*
                (lambda ()
                  (newline)
                  (display ";;; There are no more values of ")
                  (pp input)
                  (driver-loop port)))
          ((analyze input)
           (lambda (val)
             (display output-prompt)
             (pp val)
             (driver-loop port)))))))

