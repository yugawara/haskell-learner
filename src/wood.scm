(define v-span 126.33333333333)
(define v-sections 11)
(define col-width 14)
(define start-end
(lambda (span cols) (cond
((= cols 0) `((,span 0)))
(#t  (let* (
    (rest (start-end span (- cols 1)))
    (head (caar rest))
    )(cons `(,(+ 14 (+ v-span head)) ,(+ 14 head)) rest))
))))
(define v-raw (start-end v-span v-sections ) )
(define round-measurements (lambda (raw) (cond
    ((eq? raw '()) '())
    (#t (cons (list (round (caar raw)) (round (cadar raw))) (round-measurements (cdr raw))))
    )))