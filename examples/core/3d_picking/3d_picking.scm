(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(define camera (make-camera (make-vector-3 10.0 10.0 10.0)
                            (make-vector-3 0.0 0.0 0.0)
                            (make-vector-3 0.0 1.0 0.0)
                            45.0
                            camera-type/perspective))

(define cube-position (make-vector-3 0.0 1.0 0.0))

(define cube-size (make-vector-3 2.0 2.0 2.0))

(define cube-bounding-box
  (make-bounding-box
   (make-vector-3 (- (vector-3-x cube-position) (/ (vector-3-x cube-size) 2))
                  (- (vector-3-y cube-position) (/ (vector-3-y cube-size) 2))
                  (- (vector-3-z cube-position) (/ (vector-3-z cube-size) 2)))
   (make-vector-3 (+ (vector-3-x cube-position) (/ (vector-3-x cube-size) 2))
                  (+ (vector-3-y cube-position) (/ (vector-3-y cube-size) 2))
                  (+ (vector-3-z cube-position) (/ (vector-3-z cube-size) 2)))))

(defstruct state
  ray collision)

(define (main-loop current-state)
  (if (not (window-should-close?))
      (begin
        (update-camera camera)
        (begin-drawing)
        (clear-background RAYWHITE)
        (begin-mode-3d camera)
        (if (state-collision current-state)
            (begin
              (draw-cube cube-position
                         (vector-3-x cube-size)
                         (vector-3-y cube-size)
                         (vector-3-z cube-size)
                         RED)
              (draw-cube-wires cube-position
                               (vector-3-x cube-size)
                               (vector-3-y cube-size)
                               (vector-3-z cube-size)
                               MAROON)
              (draw-cube-wires cube-position
                               (+ (vector-3-x cube-size) 0.2)
                               (+ (vector-3-y cube-size) 0.2)
                               (+ (vector-3-z cube-size) 0.2)
                               GREEN))
            (begin
              (draw-cube cube-position
                         (vector-3-x cube-size)
                         (vector-3-y cube-size)
                         (vector-3-z cube-size)
                         GRAY)
              (draw-cube-wires cube-position
                               (vector-3-x cube-size)
                               (vector-3-y cube-size)
                               (vector-3-z cube-size)
                               DARKGRAY)))
        (draw-grid 10 1.0)
        (end-mode-3d)
        (draw-text "Try selecting the box with mouse!" 240 10 20 DARKGRAY)
        (when (state-collision current-state)
              (draw-text "BOX SELECTED"
                         (quotient (- screen-width (measure-text "BOX SELECTED" 30)) 2)
                         (quotient screen-height 10)
                         30
                         GREEN))
        (draw-fps 10 10)
        (end-drawing)
        (if (is-mouse-button-pressed? mouse-button/mouse-left-button)
            (main-loop (make-state ray: (get-mouse-ray (get-mouse-position) camera)
                                   collision: (check-collision-ray-box (state-ray current-state)
                                                       cube-bounding-box)))
            (main-loop (make-state ray: (get-mouse-ray (get-mouse-position) camera)
                                   collision: (state-collision current-state)))))
      (close-window)))

(init-window screen-width screen-height "raylib [core] example - 3d picking")
(set-camera-mode camera camera-mode/camera-free)
(set-target-fps 60)

(main-loop (make-state ray: (make-ray (make-vector-3 0.0 0.0 0.0)
                                      (make-vector-3 0.0 0.0 0.0))
                       collision: #f))
