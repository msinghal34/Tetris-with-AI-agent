#lang racket
(require 2htdp/universe)
(require "actions.rkt")
(require "ticks.rkt")
(require "drawing.rkt")
(require "declarations.rkt")
(require "keys.rkt")

(big-bang 
 (cons #f 0)
 (on-key (lambda(x y) (cons 1 (cdr x))))
 (on-tick (lambda(x) (if (= 0 (cdr x)) (cons (car x) 1) (cons (car x) 0))) 0.4)
 (to-draw draw-splash)
 (close-on-stop #t)
 (stop-when (lambda(x)(number?(car x))))
 (name "G-Tetris start screen"))

(big-bang
 dummyworld         	                  ;initial worldstate
 (on-tick ticker rate-expr)        ;ticker alters worldstate after every tick @ rate-expr ticks per second
 (on-key key-handler)             ;on-key alters the world after any key is pressed, acc. to key-handler
 (to-draw draw-game)	           ; renders the world after each event
 (close-on-stop #f)                ;doesn't close the window after stopping the game
 (stop-when last-world? draw-last)    ; will stop the world at the image last-pic if last-world? is #t
 (name "G-Tetris"))
 
