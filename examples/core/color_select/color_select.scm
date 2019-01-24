(include "raylib-definitions.scm")
(import defstruct
        raylib-scm
        srfi-1)
(use raylib-scm srfi-1)

(define screen-width 800)
(define screen-height 450)

(defstruct crect
  color
  selected
  rect)

(define colors
  (list DARKGRAY
        MAROON
        ORANGE
        DARKGREEN
        DARKBLUE
        DARKPURPLE
        DARKBROWN
        GRAY
        RED
        GOLD
        LIME
        BLUE
        VIOLET
        BROWN
        LIGHTGRAY
        PINK
        YELLOW
        GREEN
        SKYBLUE
        PURPLE
        BEIGE))

(define rectangles
  (map (lambda (it)
         (let ((cur-color (car it))
               (i (cadr it)))
           (make-crect color: cur-color
                       selected: #f
                       rect: (make-rectangle (+ 20 (* 100 (modulo i 7)) (* 10 (modulo i 7)))
                                             (+ 60 (* 100 (quotient i 7)) (* 10 (quotient i 7)))
                                             100
                                             100))))
       (zip colors (iota 21))))

(define (main-loop current-state)
  (if (not (window-should-close?))
      (begin
        (begin-drawing)
        (clear-background RAYWHITE)
        (for-each (lambda (r)
                    (if (crect-selected r)
                        (let* ((rect (crect-rect r))
                               (small-rect (make-rectangle (+ (rectangle-x rect) 10.0)
                                                           (+ (rectangle-y rect) 10.0)
                                                           80
                                                           80)))
                          (draw-rectangle-rec rect RAYWHITE)
                          (draw-rectangle-rec small-rect (crect-color r)))
                        (draw-rectangle-rec (crect-rect r) (crect-color r))))
                  current-state)
        (end-drawing)
        (main-loop (map (lambda (rect)
                          (let* ((c (crect-color rect))
                                 (r (color-r c))
                                 (g (color-g c))
                                 (b (color-b c)))
                            (if (check-collision-point-rec (get-mouse-position)
                                                           (crect-rect rect))
                                (make-crect color: (make-color r g b 120)
                                            selected: (not (eq? (is-mouse-button-pressed?
                                                                 mouse-button/mouse-left-button)
                                                                (crect-selected rect)))
                                            rect: (crect-rect rect))
                                (update-crect rect color: (make-color r g b 255)))))
                        current-state)))
      (close-window)))

(init-window screen-width
             screen-height
             "raylib [core] example - color selection (collision detection)")
(set-target-fps 60)

(main-loop rectangles)
