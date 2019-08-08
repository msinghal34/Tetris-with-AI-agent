#lang racket
(require 2htdp/image)
(require "declarations.rkt")
(provide draw-game)
(define canvas (rectangle 740 790 "outline" "black"))
(define (scoreboard w) (above(above  (text/font "SCORE" 36 "black" "Gill Sans" 'swiss 'normal 'bold #f) (text/font (number->string(world-score w)) 28 "black" "Gill Sans" 'swiss 'normal 'bold #f))
                             (above (text/font "HI" 36 "black" "Gill Sans" 'swiss 'normal 'bold #f) (text/font (number->string hiscore) 28 "black" "Gill Sans" 'swiss 'normal 'bold #f)))) 
(define (leveldisplay w) (above (text/font "LEVEL" 36 "black" "Gill Sans" 'swiss 'normal 'bold #f) (text/font (number->string(world-level w)) 28 "black" "Gill Sans" 'swiss 'normal 'bold #f) )) 
(define (nextpiece w)
  (cond
    [(eq? (block-type(world-nblock w)) "L1") (underlay/xy  (rectangle 50 150 "solid" (color 239 232 14)) 0 100 (rectangle 100 50 "solid" (color 239 232 14)))]
    [(eq? (block-type(world-nblock w)) "L2") (underlay/xy  (rectangle 50 150 "solid" (color 239 232 14)) -50 100 (rectangle 100 50 "solid" (color 239 232 14)))]
    [(eq? (block-type(world-nblock w)) "Z1") (underlay/xy  (rectangle 50 100 "solid" (color 45 12 153)) 50 50 (rectangle 50 100 "solid" (color 45 12 153)))]
    [(eq? (block-type(world-nblock w)) "Z2") (underlay/xy  (rectangle 50 100 "solid" (color 45 12 153)) -50 50 (rectangle 50 100 "solid" (color 45 12 153)))]
    [(eq? (block-type(world-nblock w)) "sq" )(square 100 "solid" (color 128 196 25) )]
    [(eq? (block-type(world-nblock w)) "line") (underlay/xy (rectangle 35 100 "outline" (color 0 0 0 0)) 30 0 (rectangle 50 200 "solid" (color 250 23 77)))]
    [(eq? (block-type(world-nblock w)) "T") (underlay/xy (rectangle 150 50 "solid" (color 255 153 51)) 50 50 (rectangle 50 50 "solid" (color 255 153 51)))]
    )
  )
(define (draw-grid w)
  (define n 0)
  (define g (world-grid w))
  (define c (world-cblock w))
  (define t (block-type c))
  (define grid-canvas (rectangle 500 750 "outline" (color 0 0 0 0)))
  (define col
    (cond
      [(eq? t "L1") (color 239 232 14 200)]
      [(eq? t "L2") (color 239 232 14 200)]
      [(eq? t "Z1") (color 45 12 153 200)]
      [(eq? t "Z2") (color 45 12 153 200)]
      [(eq? t "sq") (color 128 196 25 200)]
      [(eq? t "line") (color 250 23 77 200)]
      [(eq? t "T") (color 255 153 51)]
      )
    )
  (define (X x) (* 50 (modulo (- x 1) 10)))
  (define (Y x) (* 50 (quotient (- x 1) 10)))
  (begin
    (while (<=  n 149)
           (define var (vector-ref g n))
           (cond
             [(eq? 1 (cdr var)) (begin
                                             (set! grid-canvas (underlay/xy grid-canvas (X (car var)) (Y (car var)) (square 50 "solid" (color 250 23 77 200) )))
                                             (set! n (+ n 1))
                                             )]
             [(eq? 2 (cdr var)) (begin
                                             (set! grid-canvas (underlay/xy  grid-canvas (X (car var)) (Y (car var)) (square 50 "solid" (color 45 12 153 200) ) ))
                                             (set! n (+ n 1))
                                             )]
             [(eq? 3 (cdr var)) (begin
                                             (set! grid-canvas (underlay/xy  grid-canvas (X (car var)) (Y (car var)) (square 50 "solid" (color 128 196 25 200) ) ))
                                             (set! n (+ n 1))
                                             )]
             [(eq? 4 (cdr var)) (begin
                                             (set! grid-canvas (underlay/xy  grid-canvas (X (car var)) (Y (car var)) (square 50 "solid" (color 239 232 14 200) )))
                                             (set! n (+ n 1))
                                             )]
             [(eq? 5 (cdr var)) (begin
                                   (set! grid-canvas (underlay/xy  grid-canvas (X (car var)) (Y (car var)) (square 50 "solid" (color 255 153 51 200) )))
                                             (set! n (+ n 1))
                                             )]
             [else (begin
                     (set! grid-canvas (underlay/xy  grid-canvas (X (car var)) (Y (car var)) (square 50 "outline" "black")))
                     (set! n (+ n 1)))]
             )
           )
    (define j 0)
    (while (< j 4)
           (set! grid-canvas (underlay/xy grid-canvas (X (list-ref (block-lis c) j)) (Y (list-ref (block-lis c) j)) (square 50 "solid" col)))
           (set! j (+ 1 j))
           )
    grid-canvas)
  )

(define (draw-game w)
  (overlay/xy (draw-grid w) -30 -30 (overlay/xy (scoreboard w) -570 -70 (overlay/xy (leveldisplay w) -575 -265 (overlay/xy (nextpiece w) -585 -430 canvas)))))