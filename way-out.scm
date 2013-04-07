(define BASIC-DIRECTIONS
  '((up . (0 . -1))
    (right . (1 . 0))
    (down . (0 . 1))
    (left . (-1 . 0))))

(define filter
  (lambda (f l)
    (cond ((null? l) '())
	  ((f (car l)) (cons (car l) (filter f (cdr l))))
	  (else (filter f (cdr l))))))

(define get-maze
  (lambda (maze x y)
    (vector-ref (cdr maze)
		(+ (* (car maze) y) x))))

(define get-directions
  (lambda (maze x y)
    (map car
	 (filter (lambda (dir)
		   (let ((new-x (+ x (cadr dir)))
			 (new-y (+ y (cddr dir))))
		     (and (not (or (< new-x 0)
				   (< new-y 0)
				   (>= new-x (car maze))
				   (>= new-y (car maze))))
			  (not (get-maze maze new-x new-y)))))
		 BASIC-DIRECTIONS))))

(define sgn
  (lambda (n)
    (if (= n 0)
	0
	(/ n (abs n)))))

(define get-inc-directions
  (lambda (dx dy)
    (map car
	 (filter (lambda (dirs)
		   (or (and (not (= 0 (cadr dirs))) (= (cadr dirs) (sgn dx)))
		       (and (not (= 0 (cddr dirs))) (= (cddr dirs) (sgn dy)))))
		 BASIC-DIRECTIONS))))

(define set-maze
  (lambda (maze x y value)
    (vector-set! (cdr maze)
		 (+ (* (car maze) y) x) 
		 value)))

(define make-move
  (lambda (maze pos dir)
    (let* ((inc (cdr (assoc dir BASIC-DIRECTIONS)))
	   (new-pos (cons (+ (car pos) (car inc))
			  (+ (cdr pos) (cdr inc)))))
      (if (get-maze maze (car new-pos) (cdr new-pos))
	  #f
	  new-pos))))
	       
(define display-maze
  (lambda (maze)
    (let ((size (car maze)))
      (newline)
      (do ((i 0 (+ i 1)))
	  ((>= i (* size size)) (if #f #f))
	(display (if (vector-ref (cdr maze) i) "#" " "))
	(if (= (modulo (+ i 1) size) 0)
	    (newline))))))

(define boundary?
  (lambda (maze x y)
    (or (= x 0) (= y 0)
	(= x (- (car maze) 1)) (= y (- (car maze) 1)))))

;; size = velikost bludiste
;; branch faktor .. krizovatky [0,1)
;; length .. delky cest
;; hole factor [0,1] .. rozumna hodnota <= 0.5
(define generate-dungeon
  (lambda (size branch length hfac)
    (let ((toss (lambda (x) (<= (/ (random 100001) 100000) x)))
	  (maze (cons size (make-vector (* size size) #t)))
	  (holes 0))
      (define dig
	(lambda (steps incx incy x y)
	  (define rnd-sgn
	    (lambda ()
	      (set! incx (- (random 3) 1))
	      (set! incy (if (not (= incx 0))
			     0
			     (- (random 3) 1)))))
	  (define free
	    (lambda args
	      (or (null? args)
		  (and (not
			(get-maze maze 
				  (+ x (- (quotient (car args) 3) 1))
				  (+ y (- (modulo (car args) 3) 1))))
		       (apply free (cdr args))))))
	  (if (and (= incx 0) (= incy 0)) (rnd-sgn))
	  (if (not (or (not (get-maze maze x y))
		       (boundary? maze x y)
		       (free 0 1 3) (free 2 1 5) (free 5 7 8) (free 3 6 7)
		       (< steps 0)))
	      (begin
		(set-maze maze x y #f)
		(set! holes (+ holes 1))
		(if (toss branch) 
		    (let ((incx incx)
			  (incy incy))
		      (rnd-sgn)
		      (dig (- steps 1)
			   incx incy
			   (+ x incx) (+ y incy))))
		(dig (- steps 1)
		     incx incy
		     (+ x incx) (+ y incy))))))
      (do ()
	  ((>= holes (* (* size size) hfac)) maze)
	(dig (* (* size size) length)
	     0 0 
	     (+ (random (- size 2)) 1) (+ (random (- size 2)) 1))))))

(define for-all
  (lambda (f l)
    (or (null? l)
	(and (f (car l))
	     (for-all f (cdr l))))))

(define generate-position
  (lambda (maze . forbidden)
    (let ((x (random (car maze)))
	  (y (random (car maze))))
      (if (and (not (get-maze maze x y))
	       (for-all (lambda (coords)
			  (not (and (= x (car coords))
				    (= y (cdr coords)))))
			forbidden))
	  (cons x y)
	  (apply generate-position maze forbidden)))))
