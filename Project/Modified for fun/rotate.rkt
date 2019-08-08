#lang racket
(require "declarations.rkt")
(provide rotate-block collision? clear-lines lines)
(define (collision? g b)
  (let ([l (block-lis b)])
    (or (foldr (lambda (x y) (or x y)) #f (map (lambda (x) (and (< x 151) (> x 140))) l))
        (foldr (lambda (x y) (or x y)) #f (map (lambda (x) (or (number? (cdr (vector-ref g (- x 1))))(number? (cdr(vector-ref g (+ x 9)))))) l)))))
(define (clear-lines g)
  (begin
    (define (line? l) (foldr (lambda(x y)(and x y)) #t (map (lambda(x) (number? (cdr x))) l)))
    (define lines (make-vector 15 #f))
    (define i 150)
    (define copy (build-vector 150 (lambda(x)(cons (+ x 1) #f))))
    (vector-copy! copy 0 g)
    (define v (vector-take g 0))
    (while(>= i 10)
          (set! v (vector-take g 10))
          (set! i (- i 10))
          (cond [(line? (vector->list v)) (vector-set! lines (quotient (- 140 i) 10) #t)])
          (set! g (vector-take-right g i)))
    (cond
      [(foldr (lambda(x y)(or x y)) #f (vector->list lines))
       (begin
         (define line_no  (vector-memq #t lines))
         (define j (-(* 10 (+ 1 line_no))1))
         (while (>= j 10)
                (vector-set! copy j (cons (+ j 1) (cdr (vector-ref copy (- j 10)))))
                (set! j (- j 1)))
         (clear-lines copy))]
      [else copy])))
    
(define (lines g)
  (begin
    (define (line? l) (foldr (lambda(x y)(and x y)) #t (map (lambda(x) (number? (cdr x))) l)))
    (define n 0)
    (define i 150)
    (define v (vector-take g 0))
    (while(>= i 10)
          (set! v (vector-take g 10))
          (set! i (- i 10))
          (cond [(line? (vector->list v)) (set! n (+ 1 n))])
          (set! g (vector-take-right g i)))
    n))
  

(define (rotate-block g b)
  (let
      (
       (t (block-type b))
       (l (block-lis b))
       (s (block-state b))
       )
    (cond
      [(eq? t "line") (cond
                        [(= s 1) (cond
                                   [(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (and (< x 11) (> x 0))) l)) (if (number? (cdr (vector-ref g (- (+ 20 (cadr l)) 1)))) b (block t (list (cadr l) (+ 10 (cadr l)) (+ 20 (cadr l)) (+ 30 (cadr l))) 2))]
                                   [else (begin
                                           (define c (block t (list (- (cadr l) 10) (cadr l) (+ 10 (cadr l)) (+ 20 (cadr l))) 2))
                                           (while (number? (cdr (vector-ref g (- (cadddr (block-lis c)) 1))))
                                                  (set-block-lis! c (map (lambda(x) (- x 10)) (block-lis c))))
                                           (if (< (car (block-lis c)) 0) b c))])]
                        [else (cond
                                [(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (or (= 0 (modulo (- x 1) 10))(= 0 (modulo (- x 2) 10)))) l)) (block t (list (cadr l) (+ 1 (cadr l)) (+ 2 (cadr l)) (+ 3 (cadr l))) 1)]
                                [(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo x 10))) l)) (block t (list (- (cadr l) 3)(- (cadr l) 2) (- (cadr l) 1) (cadr l)) 1)]
                                [else (block t (list (- (cadr l) 2)(- (cadr l) 1) (cadr l) (+ (cadr l) 1)) 1)])])]
      [(eq? t "Z1") (cond
                      [(= s 1) (block t (list (+ (car l) 2) (caddr l) (cadddr l) (+ 10 (caddr l))) 2)]
                      [(= s 2) (cond
                                 [(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo (- x 1) 10))) l)) (block t (list (- (car l) 1) (car l) (+ 1 (cadr l)) (+ 2 (cadr l))) 1)]
                                 [else (begin
                                         (define c (block t (list (- (car l) 2) (- (car l) 1) (cadr l) (+ 1 (cadr l))) 1))
                                         (cond
                                           [(collision? g c) (begin
                                                               (define d (block t (list (- (car l) 1) (car l) (+ 1 (cadr l)) (+ 2 (cadr l))) 1))
                                                               (if (collision? g d) b d))]
                                           [else c]))])])]
                                 
      [(eq? t "Z2") (cond
                      [(= s 1) (block t (list (- (car l) 1) (caddr l) (cadddr l) (+ 20 (car l))) 2)]
                      [(= s 2) (cond
                                 [(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo (- x 1) 10))) l)) (block t (list (+ 1(cadr l)) (+ 1(caddr l)) (- (cadddr l) 1) (cadddr l)) 1)]
                                 [else (begin
                                         (define c (block t (list (cadr l) (caddr l) (- (cadddr l) 2) (- (cadddr l) 1)) 1))
                                         (cond
                                           [(collision? g c) (begin
                                                               (define d(block t (list (+ 1(cadr l)) (+ 1(caddr l)) (- (cadddr l) 1) (cadddr l)) 1))
                                                               (if (collision? g d) b d))]
                                           [else c]))])])]
      [(eq? t "T") (cond
                     [(= s 1) (if (foldr (lambda (x y) (or x y)) #f (map (lambda (x) (<= x 10)) l))
                                  (block t (list (cadr l) (+ 10 (car l)) (+ 10 (cadr l)) (+ 20 (cadr l))) 2)
                                  (block t (list (- (cadr l) 10) (car l) (cadr l) (+ 10 (cadr l))) 2))]
                     [(= s 2) (let
                                  ((c (block t (list (car l) (cadr l) (caddr l) (+ 1(caddr l))) 3)))
                                (if (or(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo x 10))) l)) (collision? g c))
                                    (block t (list (-(car l)1) (-(cadr l)1) (-(caddr l)1) (caddr l)) 3)
                                    c))]
                     [(= s 3) (block t (list (car l) (caddr l) (+ 1(caddr l)) (+ 10 (caddr l))) 4)]
                     [(= s 4) (let
                                  ((c (block t (list (- (cadr l) 1) (cadr l) (caddr l) (cadddr l)) 1)))
                                (if (or(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo (- x 1) 10))) l)) (collision? g c))
                                    (block t (list (cadr l) (+ 1(cadr l)) (+ 1(caddr l)) (+ 1(cadddr l))) 1)
                                    c))])]
      [(eq? t "L1") (cond
                      [(= s 1) (let
                                   ((c (block t (list (- (cadr l) 1) (cadr l) (+ 1 (cadr l)) (- (caddr l) 1)) 2)))
                                 (if (or(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo (- x 1) 10))) l)) (collision? g c))
                                     (block t (list (cadr l) (+ 1 (cadr l)) (+ 2 (cadr l)) (caddr l)) 2)
                                     c))]
                      [(= s 2) (let
                                   ((c (block t (list (car l) (cadr l) (+ 10 (cadr l)) (+ 20 (cadr l))) 3)))
                                 (if (collision? g c)
                                     (block t (list (- (car l)10) (-(cadr l)10) (cadr l) (+ 10 (cadr l))) 3)
                                     c))]
                      [(= s 3) (let
                                   ((c (block t (list (cadr l) (- (caddr l) 2) (- (caddr l) 1) (caddr l)) 4)))
                                 (if (or(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo (- x 1) 10))) l)) (collision? g c))
                                     (block t (list (+ 1(cadr l)) (- (caddr l) 1) (caddr l) (+ 1(caddr l))) 4)
                                     c))]
                      [(= s 4) (let ((cc (block t (list (- (caddr l) 20) (- (caddr l) 10) (caddr l) (cadddr l)) 1)))
                                 (while(< (car (block-lis cc)) 0)
                                       (set-block-lis! cc (map(lambda(x)(+ x 10)) (block-lis cc))) cc)
                                 cc)])]
      [(eq? t "L2") (cond
                      [(= s 1) (let
                                   ((c (block t (list (- (cadr l) 1) (caddr l) (+ 1 (caddr l)) (+ 2 (caddr l))) 2)))
                                 (if (or(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo x 10))) l)) (collision? g c))
                                     (block t (list (- (cadr l) 2) (-(caddr l)1) (caddr l) (+ 1 (caddr l))) 2)
                                     c))]
                      [(= s 2) (block t (list (car l) (+ 1 (car l)) (+ 10 (car l)) (+ 20 (car l))) 3)]
                      [(= s 3) (let
                                   ((c (block t (list (- (car l) 1) (car l) (+ (car l) 1) (+ 10 (cadr l))) 4)))
                                 (if (or(foldr (lambda (x y) (or x y)) #f (map (lambda (x) (= 0 (modulo (- x 1) 10))) l)) (collision? g c))
                                     (block t (list (car l) (+ 1(car l)) (+ (car l) 2) (+ 11 (cadr l))) 4)
                                     c))]
                      [(= s 4) (let
                                   ((cc (block t (list (- (caddr l) 10) (caddr l) (+ 10 (cadr l)) (+ 10 (caddr l))) 1)))
                                 (if (< (car (block-lis cc)) 0)
                                     (begin (set-block-lis! cc (map(lambda(x)(+ x 10)) (block-lis cc))) cc)
                                     cc))])]
      [else b])))
