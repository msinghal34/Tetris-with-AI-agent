;Creating a frame

  (define frame (new frame% [label "Example"][width 400][height 400]))
 
  
  
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
  
; Make a button in the frame
  (new button% [parent frame]
               [label "Click Me"]
               ; Callback procedure for a button click:
               (callback (lambda (button event)
                           (send msg set-label "Button click"))))  

  

; Make a checkbox in the frame
  (new check-box% [parent frame]
               [label "CheckBox"]
               ; Callback procedure for checkbox:
               (callback (lambda (checkbox event)
                          (if (send checkbox get-value) (send msg set-label "Checked") (send msg set-label "Unchecked") ))))  


; Make a textfield in the frame
  (new text-field% [parent frame]
               [label "Type Something"]
               ; Callback procedure for text-field:
               (callback (lambda (textbox event)
                           (send msg set-label (send textbox get-value)))))  

  
  (send frame show #t)
  

  