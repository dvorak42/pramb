(define px (lambda (s f) (let ((p (ambc p:uniform))) (if (> p 0.5) (fail) (s p)))))
(p:display-samples px 5)