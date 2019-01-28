(include "raylib-definitions.scm")
(import raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(init-window screen-width
             screen-height
             "raylib [models] example - skybox loading and drawing")

; Set our game to run at 60 frames-per-second
(set-target-fps 60)

(let* ((fp-camera (make-camera (make-vector-3 1.0 1.0 1.0)
                               (make-vector-3 4.0 1.0 4.0)
                               (make-vector-3 0.0 1.0 0.0)
                               45.0
                               camera-type/perspective))
        ; Load skybox model
       (cube (gen-mesh-cube 1.0 1.0 1.0))
       (skybox (load-model-from-mesh cube))
       (material-shader (load-shader "resources/shaders/skybox.vs"
                                     "resources/shaders/skybox.fs"))
       (cubemap-shader (load-shader "resources/shaders/cubemap.vs"
                                    "resources/shaders/cubemap.fs"))
       (tex-hdr (load-texture "resources/dresden_square.hdr")))

  ; Load skybox shader and set required locations
  ; NOTE: Some locations are automatically set at shader loading
  (set-material-shader! skybox material-shader)
  (set-shader-value! material-shader
                     (get-shader-location material-shader "environmentMap")
                     (allocate-int texmap-index/map-cubemap)
                     shader-uniform-data-type/uniform-int)

  ; Load cubemap shader and setup required shader locations
  (set-shader-value! cubemap-shader
                     (get-shader-location cubemap-shader "equirectangularMap")
                     (allocate-int 0)
                     shader-uniform-data-type/uniform-int)

  (let (; Generate cubemap (texture with 6 quads-cube-mapping) from panorama HDR texture
        ; NOTE: New texture is generated rendering to texture, shader computes the sphre->cube coordinates mapping
        (skybox (set-cubemap-texture skybox (gen-texture-cubemap cubemap-shader tex-hdr 512))))

    ; Texture not required anymore, cubemap already generated
    (unload-texture tex-hdr)
    ; Unload cubemap generation shader, not required anymore
    (unload-shader cubemap-shader)

    ; Set a first person camera mode
    (set-camera-mode fp-camera camera-mode/camera-first-person)

    (define (main-loop)
      (update-camera fp-camera)
      (if (not (window-should-close?))
          (begin
            (begin-drawing)
            (clear-background RAYWHITE)
            (begin-mode-3d fp-camera)
            (draw-model skybox
                        (make-vector-3 0.0 0.0 0.0)
                        1.0
                        WHITE)
            (draw-grid 10 10.0)
            (end-mode-3d)
            (draw-fps 10 10)
            (end-drawing)
            (main-loop))
          (begin
            (unload-model skybox)
            (close-window))))
    (main-loop)))
