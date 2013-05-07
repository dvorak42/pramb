;;; -*- Mode:Scheme -*-

(declare (usual-integrations))

(define the-unspecified-value (list 'the-unspecified-value))

(define (true? x)
  (if x true false))

(define (false? x)
  (if x false true))

;;; Primitive procedures are inherited from Scheme.

(define strict-primitive-procedure? procedure?)
(define apply-primitive-procedure apply)


;;; Compound procedures

(define (make-compound-procedure vars bproc env)
  (vector 'compound-procedure vars bproc env))

(define (compound-procedure? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'compound-procedure)))

(define (procedure-parameters p) (vector-ref p 1))
(define (procedure-body p) (vector-ref p 2))
(define (procedure-environment p) (vector-ref p 3))

;;; An ENVIRONMENT is a chain of FRAMES, made of vectors.

(define (extend-environment variables values base-environment)
  (if (fix:= (length variables) (length values))
      (vector variables values base-environment)
      (if (fix:< (length variables) (length values))
	  (error "Too many arguments supplied" variables values)
	  (error "Too few arguments supplied" variables values))))

(define (extend-environment-one variable value base-environment)
  (extend-environment (list variable) (list value) base-environment))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment '())

(define (lookup-variable-value var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	(lookup-scheme-value var)
	(let scan
	    ((vars (vector-ref env 0))
	     (vals (vector-ref env 1)))
	  (cond ((null? vars) (plp (vector-ref env 2)))
		((eq? var (car vars)) (car vals))
		(else (scan (cdr vars) (cdr vals))))))))

;;; Extension to make underlying Scheme values available to interpreter

(define (lookup-scheme-value var)
  (lexical-reference generic-evaluation-environment var))
