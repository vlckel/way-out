(require (lib "graphics.ss" "graphics"))

(load "way-out.scm")

(define BUTTON-WIDTH 110)
(define BUTTON-HEIGHT 24)
(define BUTTON-SPACING 8)

(define CELL-TYPES
  '((brick . "brick.xpm")
    (bsboy . "bsboy.xpm")
    (trsre . "trsre.xpm")))

(define display-cell
  (lambda (viewport x y type)
    ((draw-pixmap viewport)
     (cdr (assoc type CELL-TYPES))
     (make-posn (+ 4 (* 24 x))
		(+ 36 (* 24 y))))))

(define clear-cell
  (lambda (viewport x y)
    ((draw-solid-rectangle viewport)
     (make-posn (+ 4 (* 24 x))
		(+ 36 (* 24 y)))
     24 24 "black")))

(define display-maze
  (lambda (maze viewport)
    (let* ((size (car maze))
	   (pix-size (+ 8 (* size 24))))
      (do ((i 0 (+ i 1)))
	  ((>= i (* size size)) (if #f #f))
	(if (vector-ref (cdr maze) i)
	    (display-cell viewport (modulo i size) (quotient i size) 'brick)
	    (clear-cell viewport (modulo i size) (quotient i size))))
      viewport)))

(define draw-button
  (lambda (viewport pos text . emph)
    ((draw-rectangle viewport) 
     (make-posn (+ 4 (* (+ BUTTON-SPACING BUTTON-WIDTH) pos)) 5)
     BUTTON-WIDTH BUTTON-HEIGHT (if (null? emph) "pink" "tomato"))
    ((draw-string viewport) 
     (make-posn (+ 8 (* (+ BUTTON-SPACING BUTTON-WIDTH) pos))
		(- BUTTON-HEIGHT 2))
     text (if (null? emph) "pink" "tomato"))))
  
(define get-button
  (lambda (click max)
    (let* ((posn (mouse-click-posn click))
	   (x (posn-x posn))
	   (y (posn-y posn))
	   (slot (quotient (- x 8) (+ BUTTON-SPACING BUTTON-WIDTH))))
      (if (or (< y 5) (> y (+ 5 BUTTON-HEIGHT)) (< slot 0) (> slot max))
	  #f
	  slot))))

(define open-dungeon-window
  (lambda (size)
    (let* ((pix-size (+ 8 (* size 24)))
	   (viewport (open-viewport "Dungeon" pix-size (+ 24 pix-size))))
      ((draw-viewport viewport) "black")
      viewport)))

(define run-maze
  (lambda (size branch length hfac)
    (let* ((maze (generate-dungeon size branch length hfac))
	   (boy-pos #f) (boy-x #f) (boy-y #f)
	   (trs-pos #f) (trs-x #f) (trs-y #f)
	   (viewport #f)
	   (buttons '((0 . "New Maze")
		      (1 . "New Start") 
		      (2 . "Run")
		      (3 . "Quit"))))

      (define new-maze
	(lambda ()
	  (set! boy-pos #f)
	  (set! boy-x #f)
	  (set! boy-y #f)
	  (set! trs-pos #f)
	  (set! trs-x #f)
	  (set! trs-y #f)
	  (set! maze (generate-dungeon size branch length hfac))
	  (display-maze maze viewport)))

      (define new-game
	(lambda ()
	  (if boy-x
	      (begin
		(clear-cell viewport boy-x boy-y)
		(clear-cell viewport trs-x trs-y)))
	  (set! boy-pos (generate-position maze))
	  (set! boy-x (car boy-pos))
	  (set! boy-y (cdr boy-pos))
	  (set! trs-pos (generate-position maze boy-pos))
	  (set! trs-x (car trs-pos))
	  (set! trs-y (cdr trs-pos))
	  (display-cell viewport boy-x boy-y 'bsboy)
	  (display-cell viewport trs-x trs-y 'trsre)))

      (define run-loop
	(lambda (i)
	  (let* ((directions (get-directions maze boy-x boy-y))
		 (treasure (get-inc-directions (- trs-x boy-x)
					       (- trs-y boy-y)))
		 (move (eval `(navigator ',directions ',treasure))))
	    (if (and (not move))
		(begin
		  (display "Halt: ")
		  (display i)
		  (newline))
		(begin
		  (set! boy-pos (make-move maze boy-pos move))
		  (if (not boy-pos)
		      (begin
			(display "Suicide: ")
			(display i)
			(newline))
		      (begin
			(clear-cell viewport boy-x boy-y)
			(set! boy-x (car boy-pos))
			(set! boy-y (cdr boy-pos))
			(display-cell viewport boy-x boy-y 'bsboy)
			(sleep 0.1)
			(if (equal? boy-pos trs-pos)
			    (begin
			      (display "Moves: ")
			      (display i)
			      (newline))
			    (run-loop (+ i 1))))))))))

      (define event-loop
	(lambda ()
	  (for-each
	   (lambda (x)
	     (draw-button viewport (car x) (cdr x)))
	   buttons)
	  (let* ((nr (get-button (get-mouse-click viewport) 3))
		 (item (if nr (list-ref buttons nr))))
	    (if nr
		(begin
		  (draw-button viewport (car item) (cdr item) #t)
		  (sleep 0.1)
		  (draw-button viewport (car item) (cdr item))
		  (sleep 0.1)
		  (case nr
		    ((0) (new-maze) (new-game) (event-loop))
		    ((1) (new-game) (event-loop))
		    ((2) (run-loop 1))
		    ((3) #f)))
		(event-loop)))))

      (open-graphics)
      (set! viewport (open-dungeon-window (car maze)))
      (new-maze)
      (new-game)
      (event-loop)
      (close-viewport viewport)
      (close-graphics))))
