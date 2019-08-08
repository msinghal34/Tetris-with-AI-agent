#lang racket
(provide (struct-out world) (struct-out block) hiscore while rate-expr newhi dummyworld)
(define rate-expr 0.005)
(define (hiscore)
  (begin
    (define in (open-input-file "hiscore" #:mode 'text))
    (define x (string->number(read in)))
    (close-input-port in)
    x))
(struct world (grid level cblock nblock score time) #:mutable #:transparent)
(struct block (type lis state) #:mutable #:transparent)
(define (newhi x)
  (begin
    (define out (open-output-file "hiscore" #:mode 'text #:exists 'truncate))
    (write (number->string x) out)
    (close-output-port out)))

(define dummyworld (world (build-vector 150 (lambda(x) (cons (+ x 1) #f)))
                          1 (block "line" '(4 5 6 7) 1) (block "sq" '(4 5 14 15) 1)
                          0 0))
(define-syntax-rule
  (while condition statements ...)
  (begin
    (define (iter)
      (cond
        [condition (begin statements ... (iter))]))
    (iter)))
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))