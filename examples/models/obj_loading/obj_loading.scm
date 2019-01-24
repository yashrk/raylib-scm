(include "raylib-definitions.scm")
(import raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(init-window screen-width screen-height "raylib [models] example - obj model loading")

(set-trace-log (bitwise-ior trace-log-type/log-info
                            trace-log-type/log-warning
                            trace-log-type/log-error
                            trace-log-type/log-debug
                            trace-log-type/log-other))

(let* ((cur-camera (make-camera (make-vector-3 8.0 8.0 8.0)
                                (make-vector-3 0.0 2.5 0.0)
                                (make-vector-3 0.0 1.0 0.0)
                                45.0
                                camera-type/perspective))
       (position (make-vector-3 0.0 0.0 0.0))
       (castle-model (load-model "resources/models/castle.obj"))
       (castle-texture (load-texture "resources/models/castle_diffuse.png"))
       (new-castle-model (set-diffuse-texture castle-model castle-texture)))

  (define (main-loop)
    (if (not (window-should-close?))
        (begin
          (begin-drawing)

          (clear-background WHITE)

          (begin-mode-3d cur-camera)

          (draw-model new-castle-model position 0.2 WHITE)
          (draw-grid 10 1.0)
          (draw-gizmo position)

          (end-mode-3d)

          (draw-text "(c) Castle 3D model by Alberto Cano"
                     (- screen-width 200)
                     (- screen-height 20)
                     10
                     GRAY)
          (draw-fps 10 10)

          (end-drawing)

          (main-loop))

        (begin
          (unload-texture castle-texture)
          (unload-model castle-model)
          (close-window))))

  (set-target-fps 60)
  (main-loop))
