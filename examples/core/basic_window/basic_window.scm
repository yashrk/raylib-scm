(include "raylib-definitions.scm")
(import raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(init-window screen-width screen-height "raylib [core] example - basic window")
(set-target-fps 60)

(define (main-loop)
  (if (not (window-should-close?))
      (begin
        (begin-drawing)
        (clear-background RAYWHITE)
        (draw-text "Congrats! You created your first window!" 190 200 20 LIGHTGRAY)
        (end-drawing)
        (main-loop))
      (close-window)))

(main-loop)
