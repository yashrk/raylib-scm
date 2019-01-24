(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use format raylib-scm srfi-1)

(define screen-width 800)
(define screen-height 450)

(defstruct state
  frame-counter random-value)

(define (main-loop current-state)
  (if (not (window-should-close?))
      (begin
        (begin-drawing)
        (clear-background RAYWHITE)
        (draw-text "Every 2 seconds a new random value is generated:" 130 100 20 MAROON)
        (draw-text (format #f "~a" (state-random-value current-state))
                   360
                   180
                   80
                   LIGHTGRAY)
        (end-drawing)
        (main-loop (if (= (modulo (quotient (state-frame-counter current-state) 120) 2) 1)
                       (make-state frame-counter: 0
                                   random-value: (get-random-value -8 5))
                       (update-state current-state
                                     frame-counter: (+ (state-frame-counter current-state) 1)))))
      (close-window)))

(init-window screen-width screen-height "raylib [core] example - generate random values")
(set-target-fps 60)

(main-loop (make-state frame-counter: 0
                       random-value:  0))
