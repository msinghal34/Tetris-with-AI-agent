;draw curve procedure
(require (lib "plot.ss" "plot"))

(define (make-point x y)
(lambda (bit)
(if (zero? bit) x y)))

(define (x-of point)
(point 0))

(define (y-of point)
(point 1))

(define (draw curve tmin tmax)
  (line (lambda (t) 
          (vector ((x-of curve) t) ((y-of curve) t))
        )
        (mode `parametric)
        (t-max tmax)
        (t-min tmin)
  )
)


;Example
;parabola

(define (parabola tm tn)
  (define (unitx t) t)
  (define (unity t) (* t t))
  (plot (draw (make-point unitx unity) tn tm)
      ; to modify the view of the graph area change values accordingly
      (x-min -10)
      (x-max 10)
      (y-min -100)
      (y-max 100)
  )
)
