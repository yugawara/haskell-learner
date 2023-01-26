(define v-span 126.33333333333)
(define v-cols 11)
(define h-span 169.5)
(define h-cols 3)
(define col-width 14)
(define start-end
(lambda (span cols) (cond
((= cols 0) `((,span 0)))
(#t  (let* (
    (rest (start-end span (- cols 1)))
    (head (caar rest))
    )(cons `(,(+ 14 (+ span head)) ,(+ 14 head)) rest))
))))
(define round-integer (lambda (x) (flonum->fixnum (round (+ 0.0 x)))))
(define round-measurements (lambda (raw) (cond
    ((eq? raw '()) '())
    (#t (cons (list (round-integer (caar raw)) (round-integer (cadar raw))) (round-measurements (cdr raw))))
    )))