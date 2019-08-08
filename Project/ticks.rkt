#lang racket
(require "actions.rkt")
(require "declarations.rkt")
(provide ticker last-world?)
(define (exp prod a n)
  (cond
    [(= 0 n) 1]
    ((= n 1) a)
    ((= 1 (remainder n 2)) (prod a (exp prod a (- n 1))))
    (else (square prod (exp prod a (/ n 2))))
    )
  )
(define (square prod x) (prod x x))
(define (ticker w)
  (begin
    (set-world-time! w (+ (world-time w) 1))
    (if (collision? (world-grid w) (world-cblock w))
        (affix w)
        (cond
          [(= 0 (modulo (world-time w) (exact-truncate (floor (* 100 (exp * 0.75 (-(world-level w)1)))))))
           (let ((l (block-lis (world-cblock w))))
             (begin
               (set-block-lis! (world-cblock w) (map (lambda (x) (+ x 10)) l))
               (cond
                 [(collision? (world-grid w) (world-cblock w)) (affix w)]
                 )
               )
             )
           ])
        )
    w
    )
  )

(define (last-world? w)
  (letrec (
         (f (lambda (l i) (if (> i 9) l (cons (cdr(vector-ref (world-grid w) i)) (f l (+ 1 i))))))
         )
    (foldr (lambda (x y) (or x y)) #f (map number? (f '() 0)))))
