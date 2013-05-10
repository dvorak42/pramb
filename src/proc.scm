(declare (usual-integrations))

;;; Compound procedures

(define (make-compound-procedure vars bproc)
  (vector 'compound-procedure vars bproc))

(define (compound-procedure? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'compound-procedure)))

(define (procedure-parameters p) (vector-ref p 1))
(define (procedure-body p) (vector-ref p 2))
