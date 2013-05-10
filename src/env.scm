(declare (usual-integrations))

(define *env-stack*)
(define (push-env! env)
  (set! *env-stack* (cons env *env-stack*)))
(define (pop-env!)
  (let ((env (car *env-stack*)))
    (set! *env-stack* (cdr *env-stack*))
    env))
(define (peek-env) (car *env-stack*))

(define *proc-envs*)
(define (add-proc-env proc)
  (set! *proc-envs*
	(cons (cons proc (peek-env))
	      *proc-envs*)))
(define (get-proc-env proc)
  (cdr (assv proc *proc-envs*)))

(define (extend-environment variables values base-environment)
  (if (fix:= (length variables) (length values))
      (vector variables values base-environment)
      (if (fix:< (length variables) (length values))
	  (error "Too many arguments supplied" variables values)
	  (error "Too few arguments supplied" variables values))))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment '())

(define (grab-environment-state)
  (define copied-frames (list (cons the-empty-environment
				    the-empty-environment)))
  (define (copy-frame env)
    (if (not (assv env copied-frames))
	(set! copied-frames
	      (cons (cons env (extend-environment
			       (list-copy (environment-variables env))
			       (list-copy (environment-values env))
			       (copy-frame (environment-parent env))))
		    copied-frames)))
    (cdr (assv env copied-frames)))
  (cons (map copy-frame *env-stack*)
	(map (lambda (pair)
	       (cons (car pair) (copy-frame (cdr pair))))
	     *proc-envs*)))

(define (restore-environment-state state)
  (set! *env-stack* (car state))
  (set! *proc-envs* (cdr state)))

(define (lookup-variable-value var)
  (let plp ((env (peek-env)))
    (if (eq? env the-empty-environment)
	(lexical-reference generic-evaluation-environment var)
	(let scan
	    ((vars (vector-ref env 0))
	     (vals (vector-ref env 1)))
	  (cond ((null? vars) (plp (vector-ref env 2)))
		((eq? var (car vars)) (car vals))
		(else (scan (cdr vars) (cdr vals))))))))

(define (set-variable-value! var val)
  (let plp ((env (peek-env)))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let scan
            ((vars (vector-ref env 0))
             (vals (vector-ref env 1)))
          (cond ((null? vars) (plp (vector-ref env 2)))
                ((eq? var (car vars)) (set-car! vals val))
                (else (scan (cdr vars) (cdr vals))))))))

(define (define-variable! var val)
  (define env (peek-env))
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- DEFINE" var) ;should not happen.
      (let scan
	  ((vars (vector-ref env 0))
	   (vals (vector-ref env 1)))
	(cond ((null? vars)
	       (vector-set! env 0 (cons var (vector-ref env 0)))
	       (vector-set! env 1 (cons val (vector-ref env 1))))
	      ((eq? var (car vars))
	       (set-car! vals val))
	      (else
	       (scan (cdr vars) (cdr vals)))))))
