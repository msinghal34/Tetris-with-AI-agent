(define frame (new frame% [label "Drawing Example"]
                   [width 300]
                   [height 300]))

(define canvas
  (new canvas% [parent frame]
       
; paint-callback is called by default on-paint method , using the canvas and the DC returned by get-dc as the argument
       
       [paint-callback
        (lambda (canvas dc) (paint dc))]))

;A path is a set of figures defined by curves.
;A path can be used with the draw-path method of a dc<%> object to draw the path’s curves as lines,
;fill the region bounded by the path’s curves, or both. 
;A path can also be used with the set-path method of a region% object to generate a region bounded by the path’s curves.

(define rectangle-path  (new dc-path%))
(send rectangle-path move-to 50 250)
(send rectangle-path line-to 100 250)
(send rectangle-path line-to 100 100)
(send rectangle-path line-to 50 100)
(send rectangle-path close)


(define (paint dc)
  (send dc set-pen "BLUE" 5 'solid)
  
  ; (send dc set-brush "GREEN" 'solid)
  (send dc draw-path rectangle-path)
  
  )
(send frame show #t)