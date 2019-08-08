(require (lib "plot.ss" "plot"))

(define (makepoint x y)
  (lambda (p)
    (if  (= p 0)  x  y)
  )
)
(define (x-of point)
  (point 0)
)
(define (y-of point)
  (point 1)
)

(define (draw curve tmin tmax)
  (line (lambda (t)
          ( vector ((x-of curve) t) ((y-of curve) t))
          )
       (mode `parametric)
       (color `red)
       (t-max tmax)
       (t-min tmin)
  )
)

(define (vertical-line x y l)
  (define (unitx t) x)
  (define (unity t)  (+ t y))
  (plot (draw (makepoint unitx unity) 0 l)
       (y-max (+ y l 10))
  )
)