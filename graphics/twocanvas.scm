
  (define frame (new frame% [label "Two canvas"][width 400][height 400]))
 
  
  
    ; Derive a new canvas (a drawing window) class to handle events
  (define my-canvas%
    (class canvas%
      (define/override (on-event event)
        (send msg set-label (string-append (number->string (send event get-x))"," (number->string (send event get-y)))))
      (define/override (on-char event)
        (define pressed (send event get-key-code))
        (if (char? pressed) (send msg set-label (string-append  "key pressed:" (make-string 1 pressed))) ()))
      ; Call the superclass init, passing on all init args
      (super-new)))
  
  ; Make a canvas that handles events in the frame
  (define mycanvas (new my-canvas% [parent frame]))
  
 
  
  
; Make a static text message in the frame
  (define msg (new message% [parent frame]
                            [label "No events so far..."]))
  
  ; Make another canvas that handles events in the frame
  (define mycanvas (new my-canvas% [parent frame]))
  
   
  (send frame show #t)
  