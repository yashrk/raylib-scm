(include "raylib-definitions.scm")
(import defstruct
        raylib-scm
        srfi-1)
(use format
     raylib-scm
     srfi-1)

(define screen-width 800)
(define screen-height 450)

; 100K bunnies limit
(define max-bunnies       100000)
; This is the maximum amount of elements (quads) per batch
; NOTE: This value is defined in [rlgl] module and can be changed there
(define max-batch-elements 8192)

(defstruct bunny
  position
  speed
  color)

(defstruct state
  bunnies
  bunnies-count)

(init-window screen-width screen-height "raylib [textures] example - bunnymark")
(set-target-fps 60)

(define tex-bunny (load-texture "resources/wabbit_alpha.png"))

(define (main-loop current-state)
  (if (not (window-should-close?))
      (let* ((bunnies (state-bunnies current-state))
             (bunnies-count (state-bunnies-count current-state))
             (add-more (and (is-mouse-button-down? mouse-button/mouse-left-button)
                               (< bunnies-count max-bunnies)))
             (bunnies (if add-more
                          ; Create more bunnies
                          (let ((new-bunnies
                                 (map (lambda (bunny)
                                        (make-bunny
                                         position: (get-mouse-position)
                                         speed: (make-vector-2
                                                   (/ (get-random-value -250 250) 60)
                                                   (/ (get-random-value -250 250) 60))
                                         color: (make-color (get-random-value 50 240)
                                                            (get-random-value 80 240)
                                                            (get-random-value 100 240)
                                                            255)))
                                      (iota (min 100
                                                 (- max-bunnies bunnies-count))))))
                            (append bunnies new-bunnies))
                          bunnies))
             (bunnies-count (if add-more
                                (+ (min 100 (- max-bunnies bunnies-count))
                                   bunnies-count)
                                bunnies-count)))
        (begin-drawing)
        (clear-background RAYWHITE)
        ; NOTE: When internal batch buffer limit is reached (MAX_BATCH_ELEMENTS),
        ; a draw call is launched and buffer starts being filled again;
        ; before issuing a draw call, updated vertex data from internal CPU buffer is send to GPU...
        ; Process of sending data is costly and it could happen that GPU data has not been completely
        ; processed for drawing while new data is tried to be sent (updating current in-use buffers)
        ; it could generates a stall and consequently a frame drop, limiting the number
        ; of drawn bunnies
        (for-each (lambda (bunny)
                    (draw-texture tex-bunny
                                  (inexact->exact (floor (vector-2-x (bunny-position bunny))))
                                  (inexact->exact (floor (vector-2-y (bunny-position bunny))))
                                  (bunny-color bunny)))
                  bunnies)
            (draw-rectangle 0 0 screen-width 40 BLACK)
            (draw-text (format #f "bunnies: ~d" bunnies-count)
                       120
                       10
                       20
                       GREEN)
            (draw-text (format #f "batched draw calls: ~d" (/ bunnies-count max-batch-elements))
                       320
                       10
                       20
                       MAROON)
            (draw-fps 10 10)
        (end-drawing)
        ; Update bunnies
        (main-loop (update-state
                    current-state
                    bunnies: (map
                              (lambda (bunny)
                                (let* ((pos (bunny-position bunny))
                                       (speed (bunny-speed bunny))
                                       (x (vector-2-x pos))
                                       (y (vector-2-y pos))
                                       (speed-x (vector-2-x speed))
                                       (speed-y (vector-2-y speed))
                                       (new-x (+ x speed-x))
                                       (new-y (+ y speed-y))
                                       (new-speed-x (if (or (> (+ new-x
                                                                  (/ (texture-2d-width tex-bunny)
                                                                     2))
                                                               screen-width)
                                                            (< (+ new-x
                                                                  (/ (texture-2d-width tex-bunny)
                                                                     2))
                                                               0))
                                                        (* speed-x -1)
                                                        speed-x))
                                       (new-speed-y (if (or (> (+ new-y
                                                                  (/ (texture-2d-height tex-bunny)
                                                                     2))
                                                               screen-height)
                                                            (< (+ new-y
                                                                  (/ (texture-2d-height tex-bunny)
                                                                     2))
                                                               0))
                                                        (* speed-y -1)
                                                        speed-y))
                                       (new-position (make-vector-2 new-x new-y))
                                       (new-speed (make-vector-2 new-speed-x new-speed-y)))
                                  (update-bunny bunny
                                                position: new-position
                                                speed: new-speed)))
                              bunnies)
                    bunnies-count: bunnies-count)))
      (begin
        (unload-texture tex-bunny)
        (close-window))))

(main-loop (make-state bunnies: '()
                       bunnies-count: 0))
