(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use format raylib-scm)

(define screen-width 800)
(define screen-height 450)

(defstruct state
  scroll-speed position-y)

(define (main-loop current-state)
  (if (not (window-should-close?))
      (begin
        (begin-drawing)
        (clear-background RAYWHITE)
        (draw-rectangle (- (/ screen-width 2) 40)
                        (state-position-y current-state)
                        80
                        80
                        MAROON)
        (draw-text "Use mouse wheel to move the cube up and down!" 10 10 20 GRAY)
        (draw-text (format #f "Box position Y: ~3,,,' @a" (state-position-y current-state))
                   10
                   40
                   20
                   LIGHTGRAY)
        (end-drawing)
        (main-loop (update-state current-state
                                 position-y:
                                 (- (state-position-y current-state)
                                    (* (get-mouse-wheel-move)
                                       (state-scroll-speed current-state))))))
      (close-window)))

(init-window screen-width screen-height "raylib [core] example - mouse wheel")
(set-target-fps 60)

(main-loop (make-state scroll-speed: 4
                       position-y:   (- (/ screen-height 2) 40)))
