#lang racket
(require "actions.rkt")
(require "declarations.rkt")
(provide ticker last-world?)
(define (ticker w)
  (begin
    (set-world-time! w (+ (world-time w) 1))
    (cond [(last-world? w) w])
    (if (collision? (world-grid w) (world-cblock w))
        (affix w)
        (cond
          [(= 0 (modulo (world-time w) (exact-truncate (floor (* 100 (expt 0.80 6))))))
           (let ((l (block-lis (world-cblock w))))
             (begin
               (set-block-lis! (world-cblock w) (map (lambda (x) (+ x 10)) l))
               (cond
                 [(collision? (world-grid w) (world-cblock w)) (affix w)])))]))
    w))
(define (last-world? w)
  (let ((l (build-list 10 (lambda(x) (cdr (vector-ref (world-grid w) x))))))
  (foldr (lambda (x y) (or x y)) #f (map number? l))))