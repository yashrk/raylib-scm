(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use miscmacros
     raylib-scm
     regex
     srfi-1
     vlist)

(define screen-width 800)
(define screen-height 450)

(define glsl-version 330) ; must be changed to 100 on OpenGL ES 2.0 platforms

(define MAX_POSTPRO_SHADERS 12)

(define-enum postpro-shader->int int->postpro-shader
  FX_GRAYSCALE
  FX_POSTERIZATION
  FX_DREAM_VISION
  FX_PIXELIZER
  FX_CROSS_HATCHING
  FX_CROSS_STITCHING
  FX_PREDATOR_VIEW
  FX_SCANLINES
  FX_FISHEYE
  FX_SOBEL
  FX_BLOOM
  FX_BLUR)

(define postpro-shader-text
  (let* ((shader-list (map int->postpro-shader (iota 12)))
         (shader-strings (map symbol->string shader-list))
         (without-fx (map (lambda (s)
                            (string-substitute "FX_" "" s))
                          shader-strings)))
    (list->vlist without-fx)))

; Enable Multi Sampling Anti Aliasing 4x (if available)
(set-config-flags config-flag/flag-msaa-4x-hint)

(init-window screen-width screen-height "raylib [shaders] example - postprocessing shader")

(set-trace-log (bitwise-ior trace-log-type/log-info
                            trace-log-type/log-warning
                            trace-log-type/log-error
                            trace-log-type/log-debug
                            trace-log-type/log-other))

(let* ((cur-camera (make-camera (make-vector-3 2.0 3.0 2.0)
                                (make-vector-3 0.0 1.0 0.0)
                                (make-vector-3 0.0 1.0 0.0)
                                45.0
                                camera-type/perspective))
       (position (make-vector-3 0.0 0.0 0.0))
       ; Load OBJ model
       (church-model (load-model "resources/models/church.obj"))
       ; Load model texture (diffuse map)
       (church-texture (load-texture "resources/models/church_diffuse.png"))
       ; Set model diffuse texture
       (church-model (set-diffuse-texture church-model church-texture))
       ; Load all postpro shaders
       ; NOTE: All postpro shader use the base vertex shader (DEFAULT_VERTEX_SHADER)
       (shaders
        (list->vlist (list
                      (load-shader "" "resources/shaders/glsl330/grayscale.fs")
                      (load-shader "" "resources/shaders/glsl330/posterization.fs")
                      (load-shader "" "resources/shaders/glsl330/dream_vision.fs")
                      (load-shader "" "resources/shaders/glsl330/pixelizer.fs")
                      (load-shader "" "resources/shaders/glsl330/cross_hatching.fs")
                      (load-shader "" "resources/shaders/glsl330/cross_stitching.fs")
                      (load-shader "" "resources/shaders/glsl330/predator.fs")
                      (load-shader "" "resources/shaders/glsl330/scanlines.fs")
                      (load-shader "" "resources/shaders/glsl330/fisheye.fs")
                      (load-shader "" "resources/shaders/glsl330/sobel.fs")
                      (load-shader "" "resources/shaders/glsl330/bloom.fs")
                      (load-shader "" "resources/shaders/glsl330/blur.fs"))))
       ; Create a RenderTexture2D to be used for render to texture
       (target (load-render-texture screen-width screen-height)))

  (define (main-loop current-shader)
    (if (not (window-should-close?))
        (begin
          (update-camera cur-camera)

          (begin-drawing)

          (clear-background RAYWHITE)

          (begin-texture-mode target) ; Enable drawing to texture
          (clear-background RAYWHITE) ; Clear texture background
          (begin-mode-3d cur-camera)  ; Begin 3d mode drawing

          (draw-model church-model position 0.1 WHITE)
          (draw-grid 10 1.0)

          (end-mode-3d)               ; End 3d mode drawing, returns to orthographic 2d mode
          (end-texture-mode)          ; End drawing to texture (now we have a texture
                                      ; available for next passes)

          ; Render previously generated texture using selected postpro shader
          (begin-shader-mode (vlist-ref shaders current-shader))

          ; NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
          (let ((target-texture (render-texture-texture target)))
            (draw-texture-rec target-texture
                              (make-rectangle 0
                                              0
                                              (texture-2d-width target-texture)
                                              (- (texture-2d-height target-texture)))
                              (make-vector-2 0.0 0.0)
                              WHITE))

          (end-shader-mode)

          ; Draw 2d shapes and text over drawn texture
          (draw-rectangle 0 9 580 30 (fade LIGHTGRAY 0.7))

          (draw-text "(c) Church 3D model by Alberto Cano"
                     (- screen-width 200)
                     (- screen-height 20)
                     10
                     GRAY)

          (draw-text "CURRENT POSTPRO SHADER:" 10 15 20 BLACK)
          (draw-text (vlist-ref postpro-shader-text current-shader) 330 15 20 RED)
          (draw-text "< >" 540 10 30 DARKBLUE)

          (draw-fps 700 15)

          (end-drawing)

          (main-loop (cond ((is-key-pressed? keyboard-keys/key-right)
                            (modulo (+ current-shader 1) MAX_POSTPRO_SHADERS))
                           ((is-key-pressed? keyboard-keys/key-left)
                            (modulo (- current-shader 1) MAX_POSTPRO_SHADERS))
                           (else current-shader))))

        (begin
          (vlist-for-each (lambda (sh)
                            (unload-shader sh))
                          shaders)
          (unload-texture church-texture)
          (unload-model church-model)
          (unload-render-texture target)
          (close-window))))

  (set-target-fps 60)
  (set-camera-mode cur-camera camera-mode/camera-orbital)
  (main-loop FX_GRAYSCALE))
