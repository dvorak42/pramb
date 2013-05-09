(define (p:sum p1 p2)
	(lambda (succeed fail)
		(p1 (lambda (v1)
					(p2 (lambda (v2) (succeed (+ v1 v2))) fail)) fail)))

(define (p:mult p1 p2)
	(lambda (succeed fail)
		(p1 (lambda (v1)
					(p2 (lambda (v2) (succeed (* v1 v2))) fail)) fail)))

(define nbins 10)
(define ndep 10000)
(define nprec 24)
(define nstr 200)

(define (p:binify minp maxp values)
	(let lp ((bins (make-vector nbins))
					 (vs values))
		(if (= minp maxp)
				(p:draw-prob minp 0 bins)
		(if (null? vs)
				(begin (newline) (p:draw-prob minp (/ (- maxp minp) nbins) bins))
				(let ((new-bins (vector-copy bins))
							(index (min (floor->exact (/ (* nbins (- (car vs) minp)) (- maxp minp))) (- nbins 1))))
					(vector-set! new-bins index (+ (if (vector-ref new-bins index)
																						 (vector-ref new-bins index) 0) 1))
					(lp new-bins (cdr vs)))))))

(define (d-numerify v)
	(let ((vs (write-to-string (exact->inexact (/ (round (* v 100)) 100)))))
		(if (eq? (string-ref vs (- (string-length vs) 1)) #\.)
				(string-append vs "0")
				vs)))

(define (p:displayheader v)
	(let* ((l (- nprec (string-length v)))
				 (vsl (substring v 0 (min nprec (string-length v)))))
		(if (> l 0) (display (make-string l #\ )))
		(display vsl)))

(define (p:starify v)
	(if (> v 0) (display "*"))
	(if (> v (/ ndep nstr)) (p:starify (- v (/ ndep nstr)))))

(define (p:draw-prob minp step values)
	(if (= (vector-length values) 0) (newline)
			(begin
				(p:displayheader (string-append (d-numerify minp) "-" (d-numerify (+ minp step))))
				(display " ") (p:starify (vector-first values)) (newline)
				(p:draw-prob (+ minp step) step (vector-tail values 1)))))


(define (p:get-value prob) (prob (lambda (v) v) 2))

(define (p:display-helper depth minp maxp values prob)
	(let ((v (p:get-value prob)))
		(if (> depth 0)
				(p:display-helper (- depth 1) (if (< v minp) v minp) (if (> v maxp) v maxp) (cons v values) prob)
				(p:binify minp maxp values))))

(define (p:display prob)
	(p:display-helper ndep 10000000000.0 -10000000000.0 '() prob))

(p:display (lambda (s f) (s (random 100))))