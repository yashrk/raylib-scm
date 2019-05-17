(include "raylib-definitions.scm")
(import raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(init-window screen-width screen-height "raylib [models] example - heightmap loading and drawing")

(let* ((orbital-camera (make-camera (make-vector-3 18.0 16.0 18.0)
                                    (make-vector-3 0.0 0.0 0.0)
                                    (make-vector-3 0.0 1.0 0.0)
                                    45.0
                                    camera-type/perspective))

       ; Load heightmap image (RAM)
       (map-image (load-image "resources/heightmap.png"))

       ; Convert image to texture (VRAM)
       (map-texture (load-texture-from-image map-image))

       ; Generate heightmap mesh (RAM and VRAM)
       (map-mesh (gen-mesh-heightmap map-image (make-vector-3 16 8 16)))

       ; Load model from generated mesh
       (map-model (load-model-from-mesh map-mesh))

       ; Set map diffuse texture
       (map-model (set-diffuse-texture map-model 0 map-texture))

       ; Define model position
       (map-position (make-vector-3 -8.0 0.0 -8.0)))

  (define (main-loop)
    (update-camera orbital-camera)
    (if (not (window-should-close?))                   ; Detect window close button or ESC key
        (begin
          (begin-drawing)

          (clear-background RAYWHITE)

          (begin-mode-3d orbital-camera)
          (draw-model map-model map-position 1.0 RED)
          (draw-grid 20 1.0)
          (end-mode-3d)

          (draw-texture map-texture
                        (- screen-width (texture-2d-width map-texture) 20)
                        20
                        WHITE)
          (draw-rectangle-lines (- screen-width (texture-2d-width map-texture) 20)
                                20
                                (texture-2d-width map-texture)
                                (texture-2d-height map-texture)
                                GREEN)

          (draw-fps 10 10)

          (end-drawing)

          (main-loop))
        (begin
          (unload-texture map-texture)
          (unload-model map-model)
          (close-window))))

  ; Unload heightmap image from RAM, already uploaded to VRAM
  (unload-image map-image)

  ; Set an orbital camera mode
  (set-camera-mode orbital-camera camera-mode/camera-orbital)

  ; Set our game to run at 60 frames-per-second
  (set-target-fps 60)

  (main-loop))
