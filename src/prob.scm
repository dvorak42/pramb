;;; (p:value NUM_SAMPLES PROBOBJ) -> List of NUM_SAMPLES samples.
(define (p:value? exp)
  (and (pair? exp) (eq? (car exp) 'p:value)))

(define (p:get-value prob) (prob (lambda (v) v) '()))

(define (analyze-p-value exp)
  (let ((maxdepth (analyze (cadr exp)))
        (fproc (analyze (caddr exp)))
        (values '()))
    (lambda (succeed)
      (define (p-value-helper depth fproc qen)
        (fproc (lambda (proc)
          (execute-application
           proc
           (list (lambda (r)
             (succeed (if (< depth 0) qen (p-value-helper (- depth 1) fproc (cons r qen)))))
               fail)
           fail))))
      (p-value-helper (- (maxdepth (lambda (v) v)) 1) fproc '())
    )))

(defhandler analyze analyze-p-value p:value?)



(define nbins 10)
(define nprec 24)
(define nstr 200)

(define (p:binify minp maxp values)
  (define (p:binify-helper minp maxp bins vs ndep)
    (if (= minp maxp)
      (p:draw-prob minp 0 bins ndep)
      (if (null? vs)
        (begin (newline) (p:draw-prob minp (/ (- maxp minp) nbins) bins ndep))
        (let ((new-bins (vector-copy bins))
          (index (min (floor->exact (/ (* nbins (- (car vs) minp)) (- maxp minp))) (- nbins 1))))
          (vector-set! new-bins index 
            (+ (if (vector-ref new-bins index)
                   (vector-ref new-bins index) 0) 1))
          (p:binify-helper minp maxp new-bins (cdr vs) ndep))))) 
  (p:binify-helper minp maxp (make-vector nbins) values (length values)))

(define (d-numerify v)
  (let ((vs (write-to-string (exact->inexact (/ (round (* v 100)) 100)))))
    (if (eq? (string-ref vs (- (string-length vs) 1)) '#\.)
      (string-append vs "0")
      vs)))

(define (p:displayheader v)
  (let ((l (- nprec (string-length v))))
    (let ((vsl (substring v 0 (min nprec (string-length v)))))
      (if (> l 0) (display (make-string l '#\ )))
      (display vsl))))

(define (p:starify v ndep step)
  (if (> v 0) (display "*") 'eol)
  (if (> v step) (p:starify (- v step) ndep step) 'eol))

(define (p:draw-prob minp step values ndep)
  (if (= (vector-length values) 0) (newline)
    (begin
      (p:displayheader (string-append (d-numerify minp) "-" (d-numerify (+ minp step))))
      (display " ") 
      (if (vector-ref values 0)
        (p:starify (vector-ref values 0) ndep (if (/ ndep nstr) (/ ndep nstr) 0)))
      (newline)
      (p:draw-prob (+ minp step) step (vector-tail values 1) ndep))))


(define (p:display-values minp maxp values)
  (define (find-lp minp maxp values next)
    (if (= (length next) 0)
      (p:binify minp maxp values)
      (let ((v (car next)))
        (find-lp (if (< v minp) v minp) (if (> v maxp) v maxp) (cons v values) (cdr next)))))
  (find-lp minp maxp '() values))