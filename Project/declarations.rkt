#lang racket
(provide (struct-out world) (struct-out block) hiscore while rate-expr newhi)
(define rate-expr 0.01)
(define hiscore 0)
(struct world (grid level cblock nblock score time) #:mutable #:transparent)
(struct block (type lis state) #:mutable #:transparent)
(define-syntax-rule
  (while condition statements ...)
  (begin
    (define (iter)
      (cond
        [condition (begin statements ... (iter))]
        )
      )
    (iter)
    )
  )
(define (newhi x) (set! hiscore x))
