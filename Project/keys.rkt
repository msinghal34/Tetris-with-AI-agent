#lang racket
(require "declarations.rkt")
(require "actions.rkt")
(require 2htdp/universe)
(provide key-handler)
(define (key-handler w k)
  (cond
    [(key=? "left" k) (move-left w)]
    [(key=? "right" k) (move-right w)]
    [(key=? "down" k) (slam w)]
    [(key=? "up" k) (rotate w)]
    [else w]
    )
  )
