(include "raylib-definitions.scm")
(import defstruct
        raylib-scm
        srfi-1)
(use format raylib-scm srfi-1)

(define max-columns 20)

(define screen-width 800)
(define screen-height 450)

(define camera (make-camera (make-vector-3 4.0 2.0 4.0)
                            (make-vector-3 0.0 1.8 0.0)
                            (make-vector-3 0.0 1.0 0.0)
                            60.0
                            camera-type/perspective))

(defstruct column
  height position color)

(define columns
  (map (lambda (i)
         (let ((height (get-random-value 1 12)))
           (make-column height: height
                        position: (make-vector-3 (get-random-value -15 15)
                                                 (/ height 2)
                                                 (get-random-value -15 15))
                        color: (make-color (get-random-value 20 255)
                                           (get-random-value 20 255)
                                           30
                                           255))))
       (iota max-columns)))

(define cube-position (make-vector-3 0.0 0.0 0.0))

(define (main-loop)
  (if (not (window-should-close?))
      (begin
        (update-camera camera)
        (begin-drawing)
        (clear-background RAYWHITE)
        (begin-mode-3d camera)
        (draw-plane (make-vector-3 0.0 0.0 0.0)      ; Draw ground
                    (make-vector-2 32.0 32.0)
                    LIGHTGRAY)
        (draw-cube (make-vector-3 -16.0 2.5 0.0)     ; Draw a blue wall
                   1.0
                   5.0
                   32.0
                   BLUE)
        (draw-cube (make-vector-3 16.0 2.5 0.0)      ; Draw a green wall
                   1.0
                   5.0
                   32.0
                   LIME)
        (draw-cube (make-vector-3 0.0 2.5 16.0)     ; Draw a yellow wall
                   32.0
                   5.0
                   1.0
                   GOLD)
        (for-each (lambda (c)
                    (draw-cube (column-position c)
                               2.0
                               (column-height c)
                               2.0
                               (column-color c))
                    (draw-cube-wires (column-position c)
                                     2.0
                                     (column-height c)
                                     2.0
                                     MAROON))
                  columns)
        (end-mode-3d)
        (draw-rectangle 10 10 220 70 (fade SKYBLUE 0.5))
        (draw-rectangle-lines 10 10 220 70 BLUE)
        (draw-text "First person camera default controls:" 20 20 10 BLACK)
        (draw-text "- Move with keys: W, A, S, D" 40 40 10 DARKGRAY)
        (draw-text "- Mouse move to look around" 40 60 10 DARKGRAY)
        (end-drawing)
        (main-loop))
      (close-window)))

(init-window screen-width screen-height "raylib [core] example - 3d camera first person")
(set-camera-mode camera camera-mode/camera-first-person)
(set-target-fps 60)

(main-loop)
