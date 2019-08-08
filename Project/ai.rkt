#lang racket
(require "declarations.rkt")
(require "rotate.rkt")
(provide ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (join g b)
  (let(( g2 (build-vector 150 (lambda(x) (cons (+ 1 x) #f)))))
    (begin
      (vector-copy! g2 0 g)
      (map (lambda(x) (vector-set! g2 (- x 1) (cons x 1))) (block-lis b))
      g2
      )
    )
  )

(define (holes g)
  (define n 0)
  (define i 149)
  (define (check i)
    (if (= (quotient i 10) 0) #f
        (if (number? (cdr (vector-ref g (- i 10)))) #t
            (check (- i 10)))))
  (while (> i 9)
         (cond [(not (number? (cdr (vector-ref g i))))
                (cond [(check i) (set! n (+ n 1))])])
         (set! i (- i 1))
         )
  n
  )

(define (blockades g)
  (define n 0)
  (define i 0)
  (define (check-hole i)
    (cond [(= (quotient i 10) 15) #f]
          [(not (number? (cdr (vector-ref g i)))) #t]
          [else (check-hole (+ i 10))]))
  (while (<= i 139)
         (cond
           [(number? (cdr (vector-ref g i))) (cond
                                               [(check-hole (+ i 10)) (set! n (+ n 1))]
                                               )]
           )
         (set! i (+ i 1))
         )
  n
  )

(define (height-multiplier b) (foldr + 0 (map (lambda(x) (if (> x 0) (- 15 (quotient (- x 1) 10)) 0)) (block-lis b))))

(define (edge-wall b) (length (filter (lambda(x) (or (= x 1) (= x 0))) (map (lambda(x)(remainder x 10)) (block-lis b)))))

(define (edge-floor b) (length (filter (lambda(x) (= x 14)) (map (lambda(x) (quotient (- x 1) 10)) (block-lis b)))))

(define (edge-block g b)
  (define (form a)
    (define u -1)
    (define d -1)
    (define l -1)
    (define r -1)
    (cond [(not (= (quotient a 10) 0)) (set! u (- a 10))]
          [(not (= (quotient a 10) 14)) (set! d (+ a 10))]
          [(not (= remainder a 10) 0) (set! l (- a 1))]
          [(not (= remainder a 10) 9) (set! r (+ a 1))]
          )
    (if (< a 0) '() (filter (lambda(x) (not (= x -1))) (list u d l r))))
  (let*  ([aa (block-lis b)]
          [a (- (car aa) 1)]
          [b (- (cadr aa) 1)]
          [c (- (caddr aa) 1)]
          [d (- (cadddr aa) 1)]
          [l (append (form a) (form b) (form c) (form d))]
          [point (length (filter (lambda (x) (number? (cdr (vector-ref g x)))) l))])
    point))

(define (score g b)
  (begin
    (define a (clear-lines (join g b)))
    (define n (lines (join g b)))
    (define c (block (block-type b) (map (lambda(x) (+ x (* 10 n))) (block-lis b)) (block-state b)))
    (define l (reverse(map (lambda(x) (cons x (quotient (- (cadddr (block-lis c)) x) 10))) (reverse(block-lis c)))))
    (set-block-lis! c (map (lambda(x) (if (< (cdr x) n) (- 0 (car x)) (car x))) l))
    (+ (* -3.4 (height-multiplier c))
       (* -4.0 (holes a))
       (* -2.9 (blockades a))
       (* 10.0 n)
       (* 3.9 (edge-block a c))
       (* 2.0 (edge-wall c))
       (* 3.4 (edge-floor c)))

     )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ai w)
  (let*
      (
       (c (world-cblock w))
       (g (world-grid w))
       (t (block-type c))
       )
    (cond
      [(or(eq? t "line")(eq? t "Z1")(eq? t "Z1")) (tester g g c 2)]
      [(or(eq? t "L1")(eq? t "L2")(eq? t "T")) (tester g g c 4)]
      [else (tester g g c 1)]
      )
    )
  )
(define (down l ori i)
      (cond
        [(and (collision? ori (block 0 l 0)) (= i 0)) #f]
        [(and(collision? ori (block 0 l 0))(> i 0)) l]
        [else (down (map (lambda(x)(+ x 10)) l) ori (+ 1 i))]
        )
      )
(define (tester grid ori bloc states)
  (begin
    (define empty-grid (build-vector 150 (lambda(x)(cons (+ x 1) #f))))
    (define scor 0)
    (define max -100000)
    (define i 1)
    (define best (block 0 0 0))
    (define c (block (block-type bloc) (block-lis bloc) (block-state bloc)))
    (define b (block (block-type bloc) (block-lis bloc) (block-state bloc)))
    (while (<= i states)
           (set-block-lis! c (map (lambda(x) (- x (- (remainder (apply min (map(lambda(x)(remainder x 10))(block-lis c))) 10) 1))) (block-lis c)))
           (set! b (block (block-type c) (block-lis c) (block-state c)))
           (set-block-lis! b (down (block-lis b) ori 0))
           (cond
             [(not(eq? (block-lis b) #f)) (begin
                                (set! scor (score grid b))
                                (cond [(< max scor) (begin
                                                      (set! max scor)
                                                      (set! best (block (block-type b) (block-lis b) (block-state b)))
                                                      )]))]
             )
           (while (not(foldr (lambda(x y)(or x y)) #f (map (lambda(x) (= 0 (remainder x 10))) (block-lis c))))
                  (set-block-lis! c (map (lambda(x) (+ x 1)) (block-lis c)))
                  (set! b (block (block-type c) (block-lis c) (block-state c)))
                  (set-block-lis! b (down (block-lis b) ori 0))
                  (cond
                    [(not(eq? (block-lis b) #f)) (begin
                                  (set! scor (score grid b))
                                  (cond [(< max scor) (begin
                                                        (set! max scor)
                                                        (set! best (block (block-type b) (block-lis b) (block-state b)))
                                                        )]))]
                    )
                  )
           (set-block-lis! c (map (lambda(x) (- x 5)) (block-lis c)))
           (set! c (rotate-block empty-grid c))
           (set! i (+ i 1))
           )
    best
    )
  )
