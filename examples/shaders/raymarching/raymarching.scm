(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(defstruct state
  camera
  run-time)

(init-window screen-width screen-height "raylib [shaders] example - raymarching shapes")

(let* ((cur-camera (make-camera (make-vector-3 2.5 2.5 3.0)
                                (make-vector-3 0.0 0.0 0.7)
                                (make-vector-3 0.0 1.0 0.0)
                                65.0
                                camera-type/perspective))
       ; Load raymarching shader
       ; NOTE: Defining empty string for vertex shader forces usage of internal default vertex shader
       (raymarch-shader (load-shader "" "resources/shaders/glsl330/raymarching.fs"))
       ; Get shader locations for required uniforms
       (view-eye-loc (get-shader-location raymarch-shader "viewEye"))
       (view-center-loc (get-shader-location raymarch-shader "viewCenter"))
       (view-up-loc (get-shader-location raymarch-shader "viewUp"))
       (delta-time-loc (get-shader-location raymarch-shader "deltaTime"))
       (run-time-loc (get-shader-location raymarch-shader "runTime"))
       (resolution-loc (get-shader-location raymarch-shader "resolution"))
       (resolution (make-vector-2 (exact->inexact screen-width)
                                  (exact->inexact screen-height))))

  (define (main-loop current-state)
    (if (not (window-should-close?))
        (let* ((cur-camera (state-camera current-state))
               (cur-camera-pos (camera-position cur-camera))
               (cur-camera-target (camera-target cur-camera))
               (cur-camera-up (camera-up cur-camera))
               (delta-time (get-frame-time))
               (delta-time-p (allocate-float delta-time))
               (run-time (+ (state-run-time current-state)
                            delta-time))
               (run-time-p (allocate-float run-time)))
          (update-camera cur-camera)

          ; Set shader required uniform values
          (set-shader-value! raymarch-shader
                             view-eye-loc
                             cur-camera-pos
                             shader-uniform-data-type/uniform-vec3)
          (set-shader-value! raymarch-shader
                             view-center-loc
                             cur-camera-target
                             shader-uniform-data-type/uniform-vec3)
          (set-shader-value! raymarch-shader
                             view-up-loc
                             cur-camera-up
                             shader-uniform-data-type/uniform-vec3)
          (set-shader-value! raymarch-shader
                             delta-time-loc
                             delta-time-p
                             shader-uniform-data-type/uniform-float)
          (set-shader-value! raymarch-shader
                             run-time-loc
                             run-time-p
                             shader-uniform-data-type/uniform-float)

          (begin-drawing)

          (clear-background RAYWHITE)

          ; We only draw a white full-screen rectangle,
          ; frame is generated in shader using raymarching
          (begin-shader-mode raymarch-shader)
          (draw-rectangle 0 0 screen-width screen-height WHITE)
          (end-shader-mode)

          (draw-text "(c) Raymarching shader by Iñigo Quilez. MIT License."
                     (- screen-width 280)
                     (- screen-height 20)
                     10
                     GRAY)

          (end-drawing)

          (main-loop (make-state camera: cur-camera
                                 run-time: run-time)))
        (begin
          (unload-shader raymarch-shader)
          (close-window))))

  (set-camera-mode cur-camera camera-mode/camera-free)
  (set-target-fps 60)
  (set-shader-value! raymarch-shader
                     resolution-loc
                     resolution
                     shader-uniform-data-type/uniform-vec2)
  (main-loop (make-state camera: cur-camera
                         run-time: 0.0)))
