(include "raylib-definitions.scm")
(import defstruct
        format
        raylib-scm
        records)
(use format raylib-scm)

(define screen-width 800)
(define screen-height 450)

(defstruct state
  model pitch roll yaw)

(define (offset pitch)
  (define (rotate-left pitch)
    (if (< pitch -180)
        (rotate-left (+ pitch 360))
        pitch))
  (define (rotate-right pitch)
    (if (> pitch 180)
        (rotate-right (- pitch 360))
        pitch))
  (* (rotate-left (rotate-right pitch)) 10))

(define (draw-angle-gauge angle-gauge x y angle title color)
  (let* ((angle-gauge-width (texture-2d-width angle-gauge))
         (angle-gauge-height (texture-2d-height angle-gauge))
         (src-rec (make-rectangle 0 0 angle-gauge-width angle-gauge-height))
         (dst-rec (make-rectangle x y angle-gauge-width angle-gauge-height))
         (origin (make-vector-2 (/ angle-gauge-width 2)
                                (/ angle-gauge-height 2)))
         (text-size 20)
         (angle-text (format #f "~5,1f" angle)))
    (draw-texture-pro angle-gauge src-rec dst-rec origin angle color)
    (draw-text angle-text
               (- x (/ (measure-text angle-text text-size) 2))
               (+ y 10)
               text-size
               DARKGRAY)
    (draw-text title
               (- x (/ (measure-text title text-size) 2))
               (+ y 60)
               text-size
               DARKGRAY)))

(init-window screen-width
             screen-height
             "raylib [models] example - plane rotations (yaw, pitch, roll)")
(set-target-fps 60)

(set-trace-log-level trace-log-type/log-debug)

(let* ((tex-angle-gauge (load-texture "resources/angle_gauge.png"))
       (tex-background (load-texture "resources/background.png"))
       (tex-pitch (load-texture "resources/pitch.png"))
       (tex-plane (load-texture "resources/plane.png"))
       (framebuffer (load-render-texture 192 192))
       (plane-model (load-model "resources/plane.obj"))
       (tex-plane-diffuse (load-texture "resources/plane_diffuse.png"))
       (plane-model (set-diffuse-texture plane-model tex-plane-diffuse))
       (cur-camera (make-camera (make-vector-3 0.0 60.0 -120.0)
                                (make-vector-3 0.0 12.0 0.0)
                                (make-vector-3 0.0 1.0 0.0)
                                30.0
                                camera-type/perspective))
       (position (make-vector-3 0.0 6.0 0.0)))

  (define (main-loop cur-state)
    (if (not (window-should-close?))
        (let* ((cur-model (state-model cur-state))
               (pitch (state-pitch cur-state))
               (roll (state-roll cur-state))
               (yaw (state-yaw cur-state)))

          (begin-drawing)

          (clear-background RAYWHITE)

          ; Draw framebuffer texture (Ahrs Display)
          (let* ((framebuffer-texture (render-texture-texture framebuffer))
                 (center-x (/ (texture-2d-width framebuffer-texture) 2))
                 (center-y (/ (texture-2d-height framebuffer-texture) 2))
                 (scale-factor 0.5)
                 (pitch-offset (offset pitch)))
            (begin-texture-mode framebuffer)

            (begin-blend-mode blend-mode/blend-alpha)

            (draw-texture-pro tex-background
                              (make-rectangle 0
                                              0
                                              (texture-2d-width tex-background)
                                              (texture-2d-height tex-background))
                              (make-rectangle center-x
                                              center-y
                                              (* (texture-2d-width tex-background) scale-factor)
                                              (* (texture-2d-height tex-background) scale-factor))
                              (make-vector-2 (* (/ (texture-2d-width tex-background) 2)
                                                scale-factor)
                                             (+ (* (/ (texture-2d-height tex-background) 2)
                                                   scale-factor)
                                                (* pitch-offset scale-factor)))
                              roll
                              WHITE)

            (draw-texture-pro tex-pitch
                              (make-rectangle 0
                                              0
                                              (texture-2d-width tex-pitch)
                                              (texture-2d-height tex-pitch))
                              (make-rectangle center-x
                                              center-y
                                              (* (texture-2d-width tex-pitch) scale-factor)
                                              (* (texture-2d-height tex-pitch) scale-factor))
                              (make-vector-2 (* (/ (texture-2d-width tex-pitch) 2) scale-factor)
                                             (+ (* (/ (texture-2d-height tex-pitch) 2)
                                                   scale-factor)
                                                (* pitch-offset scale-factor)))
                              roll
                              WHITE)

            (draw-texture-pro tex-plane
                              (make-rectangle 0
                                              0
                                              (texture-2d-width tex-plane)
                                              (texture-2d-height tex-plane))
                              (make-rectangle center-x
                                              center-y
                                              (* (texture-2d-width tex-plane) scale-factor)
                                              (* (texture-2d-height tex-plane) scale-factor))
                              (make-vector-2 (* (/ (texture-2d-width tex-plane) 2)
                                                scale-factor)
                                             (* (/ (texture-2d-height tex-plane) 2)
                                                scale-factor))
                              0
                              WHITE)

            (draw-rectangle-lines (- screen-width (texture-2d-width framebuffer-texture) 20)
                                  20
                                  (texture-2d-width framebuffer-texture)
                                  (texture-2d-height framebuffer-texture)
                                  DARKGRAY)

            (end-blend-mode)

            (end-texture-mode))

          ;Draw 3D model (recomended to draw 3D always before 2D)
          (begin-mode-3d cur-camera)

          (draw-model cur-model position 1.0 WHITE)
          (draw-grid 10 10.0)

          (end-mode-3d)

          ;Draw 2D GUI stuff
          (draw-angle-gauge tex-angle-gauge 80 70 roll "roll" RED)
          (draw-angle-gauge tex-angle-gauge 190 70 pitch "pitch" GREEN)
          (draw-angle-gauge tex-angle-gauge 300 70 yaw "yaw" SKYBLUE)

          (draw-rectangle 30 360 260 70 (fade SKYBLUE 0.5))
          (draw-rectangle-lines 30 360 260 70 (fade DARKBLUE 0.5))
          (draw-text "Pitch controlled with: KEY_UP / KEY_DOWN" 40 370 10 DARKGRAY)
          (draw-text "Roll controlled with: KEY_LEFT / KEY_RIGHT" 40 390 10 DARKGRAY)
          (draw-text"Yaw controlled with: KEY_A / KEY_S" 40 410 10 DARKGRAY)

          ; Draw framebuffer texture
          (let* ((framebuffer-texture (render-texture-texture framebuffer))
                 (texture-width (texture-2d-width framebuffer-texture))
                 (texture-height (texture-2d-height framebuffer-texture)))
            (draw-texture-rec framebuffer-texture
                              (make-rectangle 0
                                              0
                                              texture-width
                                              (- texture-height))
                              (make-vector-2 (- screen-width texture-width 20) 20)
                              (fade WHITE 0.8)))

          (end-drawing)

          (let* ((new-roll (cond ((is-key-down? keyboard-keys/key-left) (+ roll 1.0))
                                 ((is-key-down? keyboard-keys/key-right) (- roll 1.0))
                                 ((> roll 0.0) (- roll 0.5))
                                 ((< roll 0.0) (+ roll 0.5))
                                 (else roll)))
                 (new-yaw (cond ((is-key-down? keyboard-keys/key-s) (+ yaw 1.0))
                                ((is-key-down? keyboard-keys/key-a) (- yaw 1.0))
                                ((> yaw 0.0) (- yaw 0.5))
                                ((< yaw 0.0) (+ yaw 0.5))
                                (else yaw)))
                 (new-pitch (cond ((is-key-down? keyboard-keys/key-down) (+ pitch 0.6))
                                  ((is-key-down? keyboard-keys/key-up) (- pitch 0.6))
                                  ((> pitch 0.3) (- pitch 0.3))
                                  ((< pitch -0.3) (+ pitch 0.3))
                                  (else pitch))))
            (if (or (> (abs (- new-pitch 0.3)) flonum-epsilon)
                    (> (abs new-roll) flonum-epsilon)
                    (> (abs new-yaw) flonum-epsilon))
                (let* ((transform (matrix-identity))
                       (transform (matrix-multiply transform
                                                   (matrix-rotate-z (deg->rad new-roll))))
                       (transform (matrix-multiply transform
                                                   (matrix-rotate-x (deg->rad new-pitch))))
                       (transform (matrix-multiply transform
                                                   (matrix-rotate-y (deg->rad new-yaw))))
                       (new-model (set-transform cur-model transform)))
                  (main-loop (make-state model: new-model
                                         pitch: new-pitch
                                         roll:  new-roll
                                         yaw:   new-yaw)))
                (main-loop cur-state))))
        (begin
          (unload-model plane-model)
          (unload-render-texture framebuffer)
          (unload-texture tex-angle-gauge)
          (unload-texture tex-background)
          (unload-texture tex-pitch)
          (unload-texture tex-plane)
          (close-window))))

  (gen-texture-mipmaps (get-diffuse-texture plane-model))
  (main-loop (make-state model: plane-model
                         pitch: 0.0
                         roll:  0.0
                         yaw:   0.0)))
