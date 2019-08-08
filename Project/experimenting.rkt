
#lang racket
(require 2htdp/universe)
(require "actions.rkt")
(require "ticks.rkt")
(require "drawing.rkt")
(require "declarations.rkt")
(require "keys.rkt")

(define (g-helper i)
  (if (= i 151) '()
      (cons (cons i #f) (g-helper (+ i 1)))))
(define g
  (g-helper 1))
(define v (list->vector g))
(define b (block "L2" '(5 15 24 25) 1))
(define c (block "line" '(4 5 6 7) 1))
(define dummyworld (world v 1 b c 0 0))
         
(big-bang
 dummyworld
 (on-tick ticker rate-expr)
 (to-draw draw-game)
 (on-key key-handler)
 (stop-when last-world?)
 )
