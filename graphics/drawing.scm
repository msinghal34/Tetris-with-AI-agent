  ; Make a 300 x 300 frame
  (define frame (new frame% [label "Drawing Example"]
                            [width 300]
                            [height 300]))
  
  ; Make the drawing area with a paint callback
  (define canvas
    (new canvas% [parent frame]
                 [paint-callback
                  (lambda (canvas dc) (paint dc))]))
  
  ; ... pens, brushes, and draw-face are the same as above ...
  
 (define (paint dc) (send dc draw-bitmap face-bitmap 0 0))

   ; ... pens, brushes, and draw-face are the same as above ...
  
  ; Create a 300 x 300 bitmap
  (define face-bitmap (make-object bitmap% 300 300))
  ; Create a drawing context for the bitmap
  (define bm-dc (make-object bitmap-dc% face-bitmap))
  ; A bitmap's initial content is undefined; clear it before drawing
  (send bm-dc clear)
  
  
 
  ; Make some pens and brushes
  (define black-pen (make-object pen% "BLACK" 1 'solid))
  (define no-brush (make-object brush% "BLACK" 'transparent))
  (define blue-brush (make-object brush% "BLUE" 'solid))
  (define yellow-brush (make-object brush% "YELLOW" 'solid))
  (define red-pen (make-object pen% "RED" 2 'solid))
  
  ; Define a procedure to draw a face
  
 (define (draw-face dc x)
    (send dc set-pen black-pen)
    (send dc set-brush no-brush)
    (send dc draw-ellipse 50 50 200 200)
  
    (send dc set-brush yellow-brush)
    (send dc draw-rectangle 100 100 10 10)
    (send dc draw-rectangle 200 100 10 10)
  
    (send dc set-brush no-brush)
    (send dc set-pen red-pen)
    (send dc draw-arc 75 75 150 150 (+ pi x) (- (* 2 pi) x) ))
  
 
 

 ; Show the frame
  (send frame show #t)
  
  (define (draw-animated i)
   
    (cond ((= i -1)
           
             (send canvas refresh)           
           )
          (else   
             (send bm-dc clear)
             (draw-face bm-dc (* (/ i 16) pi))
             (send canvas refresh)
             (sleep/yield 0.3)
             (draw-animated (- i 1))
                            )) 
  )
  
  
  (draw-animated 4)
  