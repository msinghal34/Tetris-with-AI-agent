#lang racket
;importing libraries
(require 2htdp/universe)
(require "drawing.rkt")
(require "declarations.rkt")


;conditionally execute big-bangs???
(big-bang
 initial-state                    ;initial worldstate
 (on-tick ticker rate-expr)         ;ticker alters worldstate after every tick @ rate-expr ticks per second
 (on-key key-handler)               ;on-key alters the world after any key is pressed, acc. to key-handler
 (to-draw draw-game)    ; renders the world after each event
 (close-on-stop #f)                 ;doesnt close the window after stopping the game
 ;(stop-when last-world? last-pic)    ; will stop the world at the image last-pic if last-world? is #t
 (name "G-tetris")
 )

 