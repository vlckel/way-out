(load "way-out-graphics.scm")

(define navigator
  (lambda (directions treasure-dir)
    (list-ref directions
              (random (length directions)))))

(run-maze 20 .9 1 .05)
;(run-maze 20 .1 .1 .5)
;(run-maze 20 .1 .9 .4)
;(run-maze 40 .01 1 .45)
;(run-maze 40 .01 .1 .45)


