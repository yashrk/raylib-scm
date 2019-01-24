(include "raylib-definitions.scm")
(import raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(define camera (make-camera (make-vector-3 0.0 10.0 10.0)
                            (make-vector-3 0.0 0.0 0.0)
                            (make-vector-3 0.0 1.0 0.0)
                            45.0
                            camera-type/perspective))

(define cube-position (make-vector-3 0.0 0.0 0.0))

(define (main-loop)
  (if (not (window-should-close?))
      (begin
        (begin-drawing)
        (clear-background RAYWHITE)
        (begin-mode-3d camera)
        (draw-cube cube-position 2.0 2.0 2.0 RED)
        (draw-cube-wires cube-position 2.0 2.0 2.0 MAROON)
        (draw-grid 10 1.0)
        (end-mode-3d)
        (draw-text "Welcome to the third dimension!" 10 40 20 DARKGRAY)
        (draw-fps 10 10)
        (end-drawing)
        (main-loop))
      (close-window)))

(init-window screen-width screen-height "raylib [core] example - 3d mode")
(set-target-fps 60)

(main-loop)
