;;; draw.scm -  Copyright 1996 Dave Mason, Toronto, Canada - all rights reserved

(require 'module)
(module draw
   (export draw-move draw-line draw-point draw-text draw-clear draw-rgb draw-fill
	   draw-scale draw-scale-cm draw-scale-inch draw-scale-pt 
	   draw-scale-x-log draw-scale-y-log draw-scale-x-linear draw-scale-y-linear
	   draw-start draw-end draw-font draw-font-size draw-line-width
	   draw-eps)
   (define draw-port #f)
   (define draw-file "draw.ps")
   (define n-pages 0)
   (define font "Helvetica")
   (define font-list '())
   (define font-size 14)
   (define current-font #f)
   (define current-font-size #f)
   (define page-name #f)
   (define some-stuff #f)
   (define is-path #f)
   (define ps-version "")
   (define ps-comments "")
   (define title #f)
   (define scale-x 1)
   (define scale-y 1)
   (define translate-x-default 70)
   (define translate-y-default 120)
   (define translate-x 70)
   (define translate-y 120)
   (define adjust-x (lambda (x) x))
   (define adjust-y (lambda (x) x))
   (define encapsulated #f)
   (define bblx  1000000)
   (define bbhx -1000000)
   (define bbly  1000000)
   (define bbhy -1000000)
   (define line-join 0)
   (define line-cap 0)
   (define init-port
     (lambda ()
       (or draw-port
	   (let ((port (open-output-file draw-file)))
	     (set! draw-port port)
	     (display "%!PS-Adobe-3.0" port)
	     (display ps-version port)
	     (display "\n%%Creator: draw.scm Copyright 1996 Dave Mason\n" port)
	     (if title
		 (begin
		   (display "%%Title: " port)
		   (display title port)
		   (display "\n" port)))
	     (display ps-comments port)
	     (display "%%LanguageLevel: 2
%%Pages: (atend)
%%BoundingBox: (atend)
%%DocumentNeededResources: (atend)
%%EndComments
%%BeginProlog
/msave { matrix currentmatrix } def
/set  { setmatrix } def
/tra  { translate } def
/sca  { scale } def
/rot  { rotate } def
/cc   { concat } def
/m    { moveto } def
/l    { lineto } def
/r    { rlineto } def
/p    { moveto 0 0 rlineto } def
/s    { currentpoint stroke moveto } def
/selectfont  { exch findfont exch scalefont setfont } def
%%EndProlog
" port)
	     port))))
   (define port
     (lambda ()
       (let ((port (init-port)))
	 (if some-stuff
	     port
	     (begin
	       (set! some-stuff #t)
	       (display "%%Page: " port)
	       (if page-name
		   (begin
		     (display page-name port)
		     (display " " port)))
	       (set! n-pages (+ 1 n-pages))
	       (display n-pages port)
	       (display "
%%BeginPageSetup
/pgsave save def
%%EndPageSetup
" port)
	       port)))))
   (define draw-scale-cm
     (lambda ()
       (set! scale-x (/ 72 2.54))
       (set! scale-y (/ 72 2.54))))
   (define draw-scale-inch
     (lambda ()
       (set! scale-x 72)
       (set! scale-y 72)))
   (define draw-scale-pt
     (lambda ()
       (set! scale-x 1)
       (set! scale-y 1)))
   (define draw-scale
     (lambda (x y)
       (set! scale-x (* scale-x x))
       (set! scale-y (* scale-y y))))
   (define draw-scale-x-log
     (lambda (low . optional)
       (set! translate-x (- (if (pair? optional)
				(* (car optional) scale-x)
				translate-x-default)
			    (* (log low) scale-x)))
       (set! adjust-x log)))
   (define draw-scale-y-log
     (lambda (low . optional)
       (set! translate-y (- (if (pair? optional)
				(* (car optional) scale-y)
				translate-y-default)
			    (* (log low) scale-y)))
       (set! adjust-y log)))
   (define draw-scale-x-linear
     (lambda (low . optional)
       (set! translate-x (- (if (pair? optional)
				(* (car optional) scale-x)
				translate-x-default)
			    (* low scale-x)))
       (set! adjust-x (lambda (x) x))))
   (define draw-scale-y-linear
     (lambda (low . optional)
       (set! translate-y (- (if (pair? optional)
				(* (car optional) scale-y)
				translate-y-default)
			    (* low scale-y)))
       (set! adjust-y (lambda (x) x))))
   (define display-char
     (lambda (port)
       (lambda (c)
	 (case c
	   ((#\\ #\( #\)) (display #\\ port) (display c port))
	   (else (display c port))))))
   (define draw-font
     (lambda (f)
       (set! font f)))
   (define draw-font-size
     (lambda (s)
       (set! font-size s)))
   (define init-font
     (lambda ()
       (if (not (member font font-list))
	   (let ((port (port)))
	     (set! font-list (cons font font-list))
	     (display "%%IncludeResource: font " port)
	     (display font port)
	     (display "\n" port)))
       (if (not (and (equal? font current-font)
		     (equal? font-size current-font-size)))
	   (let ((port (port)))
	     (display "/" port)
	     (display font port)
	     (display " " port)
	     (display font-size port)
	     (display " selectfont\n" port)
	     (set! current-font font)
	     (set! current-font-size font-size)))))
   (define draw-rgb
     (lambda (r g b)
       (let ((port (port)))
	 (if is-path (display "s\n" port))
	 (set! is-path #f)
	 (display r port) (display " " port)
	 (display g port) (display " " port)
	 (display b port) (display " " port)
	 (display "setrgbcolor\n" port))))
   (define draw-fill
     (lambda ()
       (if is-path (display "fill\n" (port)))
       (set! is-path #f)))
   (define draw-text
     (lambda (text)
       (let ((port (port)))
	 (init-font)
	 (set! is-path #t)
	 (display "(" port)
	 (letrec
	     ((disp
	       (lambda (text)
		 (if (or (symbol? text) (string? text) (char? text))
		     (for-each (display-char port)
			       (cond
				((symbol? text)
				 (string->list (symbol->string text)))
				((char? text) (list text))
				((string? text) (string->list text))))
		     (if (pair? text)
			 (begin
			   (display "\\(" port)
			   (disp-list #f text)
			   (display "\\)" port)
			   )
			 (if (null? text)
			     (display "\\(\\)" port)
			     (display text port))))))
	      (disp-list
	       (lambda (space list)
		 (if space (display " " port))
		 (disp (car list))
		 (if (pair? (cdr list))
		     (disp-list #t (cdr list))
		     (if (not (null? (cdr list)))
			 (begin
			   (display " . " port)
			   (disp (cdr list))))))))
	   (disp text))
	 (display ")show\n" port))))
   (define disp-x
     (lambda (n port optional)
       (let ((n (+ translate-x
		   (* (adjust-x n) scale-x)
		   (if (pair? optional) (car optional) 0))))
	 (if (< n bblx) (set! bblx (if (inexact? n)
				       (inexact->exact (- n 0.5))
				       n)))
	 (if (> n bbhx) (set! bbhx (if (inexact? n)
				       (inexact->exact (+ n 0.5))
				       n)))
	 (display
	  (if (inexact? n)
	      (/ (inexact->exact (* n 1000)) 1000)
	      n)
	  port))))
   (define disp-y
     (lambda (n port optional)
       (let ((n (+ translate-y
		   (* (adjust-y n) scale-y)
		   (if (and (pair? optional) (pair? (cdr optional)))
		       (cadr optional) 0))))
	 (if (< n bbly) (set! bbly (if (inexact? n)
				       (inexact->exact (- n 0.5))
				       n)))
	 (if (> n bbhy) (set! bbhy (if (inexact? n)
				       (inexact->exact (+ n 0.5))
				       n)))
	 (display
	  (if (inexact? n)
	      (/ (inexact->exact (* n 1000)) 1000)
	      n)
	  port))))
   (define do-xy
     (lambda (what x y . optional)
       (let ((port (port)))
	 (disp-x x port optional)
	 (display " " port)
	 (disp-y y port optional)
	 (display what port))))
   (define draw-move
     (lambda x
       (if is-path (display "s\n" (port)))
       (set! is-path #f)
       (apply do-xy (cons " m\n" x))))
   (define draw-line
     (lambda x
       (cap-style 0)
       (set! is-path #t)
       (apply do-xy (cons " l\n" x))))
   (define draw-point
     (lambda x
       (cap-style 1)
       (set! is-path #t)
       (apply do-xy (cons " p\n" x))))
   (define join-style
     (lambda (n)
       (if (not (= n line-join))
	   (let ((port (port)))
	     (display n port)
	     (display " setlinejoin\n" port)
	     (set! line-join n)))))
   (define draw-line-width
     (lambda (n)
       (let ((port (port)))
	 (if is-path (display "s\n" port))
	 (set! is-path #f)
	 (display n port)
	 (display " setlinewidth\n" port))))
   (define cap-style
     (lambda (n)
       (if (not (= n line-cap))
	   (let ((port (port)))
	     (display "s\n" port)
	     (display n port)
	     (display " setlinecap\n" port)
	     (set! line-cap n)))))
   (define draw-start
     (lambda (file . rest)
       (draw-end)
       (set! draw-file file)
       (if (pair? rest)
	   (set! title (car rest)))))
   (define draw-eps
     (lambda ()
       (set! ps-version " EPSF 3.0")
       (set! encapsulated #t)))
   (define draw-end
     (lambda ()
       (if draw-port
	   (let ((port draw-port))
	     (draw-clear)
	     (display "%%Trailer\n%%Pages: " port)
	     (display n-pages port)
	     (display "\n%%DocumentNeededResources: " port)
	     (if (pair? font-list)
		 (for-each (lambda (x) (display x port) (display " " port))
		   (cons "font" font-list)))
	     (display "\n%%BoundingBox: " port)
	     (for-each (lambda (x) (display x port) (display " " port))
		       (list bblx bbly bbhx bbhy))
	     (display "\n%%EOF\n" port)
	     (close-output-port port)
	     (set! draw-port #f)))))
   (define draw-clear
     (lambda x
       (if some-stuff
	   (let ((port draw-port))
	     (if is-path (display "s\n" port))
	     (set! is-path #f)
	     (display "pgsave restore showpage\n" port)
	     (set! some-stuff #f)
	     (set! line-cap 0)
	     (set! current-font #f)
	     (set! current-font-size #f)
	     ))
       (set! page-name
	     (if (and (pair? x) (or (string? (car x))
				    (symbol? (car x))))
		 (car x)
		 #f))))
)
(provide 'draw)
