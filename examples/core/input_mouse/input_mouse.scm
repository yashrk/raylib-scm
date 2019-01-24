(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use format raylib-scm)

(define screen-width 800)
(define screen-height 450)

(defstruct state
  color position)

(define (main-loop current-state)
  (if (not (window-should-close?))
      (begin
        (begin-drawing)
        (clear-background RAYWHITE)
        (draw-text "move ball with mouse and click mouse button to change color" 10 10 20 DARKGRAY)
        (draw-circle-v (state-position current-state) 40.0 (state-color current-state))
        (end-drawing)
        (main-loop (make-state color:
                               (cond ((is-mouse-button-pressed? mouse-button/mouse-left-button)
                                      MAROON)
                                     ((is-mouse-button-pressed? mouse-button/mouse-middle-button)
                                      LIME)
                                     ((is-mouse-button-pressed? mouse-button/mouse-right-button)
                                      DARKBLUE)
                                     (else (state-color current-state)))
                               position: (get-mouse-position))))
      (close-window)))

(init-window screen-width screen-height "Raylib window")
(set-target-fps 60)

(main-loop (make-state color: DARKBLUE
                       position: (make-vector-2 -100.0 -100.0)))
