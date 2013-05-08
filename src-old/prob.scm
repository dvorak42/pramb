(define (p:sum p1 p2)
	(lambda (succeed fail)
		(p1 (lambda (v1)
					(p2 (lambda (v2) (succeed (+ v1 v2))) fail)) fail)))

(define (p:mult p1 p2)
	(lambda (succeed fail)
		(p1 (lambda (v1)
					(p2 (lambda (v2) (succeed (* v1 v2))) fail)) fail)))

(define (p:get-value prob)
  (prob (lambda (v) 
v) 2))

(define nbins 10)
(define ndep 100000)
(define nprec 8)
(define nstr 100)

(define (p:binify minp maxp values)
	(let lp ((bins (make-vector nbins))
					 (vs values))
		(if (null? vs)
				(p:draw-prob minp (/ (- maxp minp) nbins) bins)
				(let ((new-bins (vector-copy bins))
							(index (min (floor->exact (/ (* nbins (- (car vs) minp)) (- maxp minp))) (- nbins 1))))
					(vector-set! new-bins index (+ (if (vector-ref new-bins index)
																						 (vector-ref new-bins index) 0) 1))
					(lp new-bins (cdr vs))))))

(define (p:display-v v)
	(let* ((vs (write-to-string (exact->inexact (/ (round (* v 10)) 10))))
				 (l (- nprec (string-length vs)))
				 (vsl (substring vs 0 (min nprec (string-length vs)))))
		(if (> l 0) (display (make-string l #\ )))
		(display vsl)))

(define (p:display-n v)
	(if (> v 0) (display "*"))
	(if (> v (/ ndep nstr)) (p:display-n (- v (/ ndep nstr)))))

(define (p:draw-prob minp step values)
	(if (= (vector-length values) 0) (newline)
			(begin
				(p:display-v minp); (display "-") (p:display-v (+ minp step))
				(display " ") (p:display-n (vector-first values)) (newline)
				(p:draw-prob (+ minp step) step (vector-tail values 1)))))

(define (p:display prob)
	(let lp ((depth ndep)
					 (minp 10000000.0)
					 (maxp -10000000.0)
					 (values '()))
		(let ((v (p:get-value prob)))
			(if (> depth 0)
					(lp (- depth 1) (if (< v minp) v minp) (if (> v maxp) v maxp) (cons v values))
					(p:binify minp maxp values)))))


	