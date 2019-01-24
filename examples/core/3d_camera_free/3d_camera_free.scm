(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(define cube-position (make-vector-3 0.0 0.0 0.0))

(defstruct state
  camera)

(define (main-loop current-state)
  (if (not (window-should-close?))
      (let ((cur-camera (state-camera current-state)))
        (update-camera cur-camera)

        (begin-drawing)

        (clear-background RAYWHITE)

        (begin-mode-3d cur-camera)
        (draw-cube cube-position 2.0 2.0 2.0 RED)
        (draw-cube-wires cube-position 2.0 2.0 2.0 MAROON)
        (draw-grid 10 1.0)
        (end-mode-3d)

        (draw-rectangle 10 10 320 133 (fade SKYBLUE 0.5))
        (draw-rectangle-lines 10 10 320 133 BLUE)

        (draw-text "Free camera default controls:" 20 20 10 BLACK)
        (draw-text "- Mouse Wheel to Zoom in-out" 40 40 10 DARKGRAY)
        (draw-text "- Mouse Wheel Pressed to Pan" 40 60 10 DARKGRAY)
        (draw-text "- Alt + Mouse Wheel Pressed to Rotate" 40 80 10 DARKGRAY)
        (draw-text "- Alt + Ctrl + Mouse Wheel Pressed for Smooth Zoom" 40 100 10 DARKGRAY)
        (draw-text "- Z to zoom to (0, 0, 0)" 40 120 10 DARKGRAY)

        (end-drawing)

        (main-loop (if (is-key-down? (char->integer #\Z))
                       (update-state current-state
                                     camera:
                                     (make-camera (make-vector-3 10.0 10.0 10.0)
                                                  (make-vector-3 0.0 0.0 0.0)
                                                  (make-vector-3 0.0 1.0 0.0)
                                                  45.0
                                                  camera-type/perspective))
                       current-state)))
      (close-window)))

(init-window screen-width screen-height "raylib [core] example - 3d camera free")

(let ((cur-camera (make-camera (make-vector-3 10.0 10.0 10.0)
                           (make-vector-3 0.0 0.0 0.0)
                           (make-vector-3 0.0 1.0 0.0)
                           45.0
                           camera-type/perspective)))
  (set-camera-mode cur-camera camera-mode/camera-free)
  (set-target-fps 60)
  (main-loop (make-state camera: cur-camera)))
