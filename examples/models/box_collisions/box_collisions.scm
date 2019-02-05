(include "raylib-definitions.scm")
(import defstruct
        format
        raylib-scm
        records)
(use format raylib-scm)

(define screen-width 800)
(define screen-height 450)

(defstruct state
  player-position
  player-color)

(init-window screen-width
             screen-height
             "raylib [models] example - box collisions")
(set-target-fps 60)

(set-trace-log-level trace-log-type/log-debug)

(let* ((cur-camera (make-camera (make-vector-3 0.0 10.0 10.0)
                                (make-vector-3 0.0 0.0 0.0)
                                (make-vector-3 0.0 1.0 0.0)
                                45.0
                                camera-type/perspective))
       (player-size (make-vector-3 1.0 2.0 1.0))
       (enemy-box-pos (make-vector-3 -4.0 1.0 0.0))
       (enemy-box-size (make-vector-3 2.0 2.0 2.0))
       (enemy-sphere-pos (make-vector-3 4.0 0.0 0.0))
       (enemy-sphere-size 1.5))

  (define (main-loop cur-state)
    (if (not (window-should-close?))
        (let* ((player-position (state-player-position cur-state))
               (player-color (state-player-color cur-state))
               (new-position (cond ((is-key-down? keyboard-keys/key-right)
                                    (make-vector-3
                                     (+ (vector-3-x player-position) 0.2)
                                     (vector-3-y player-position)
                                     (vector-3-z player-position)))
                                   ((is-key-down? keyboard-keys/key-left)
                                    (make-vector-3
                                     (- (vector-3-x player-position) 0.2)
                                     (vector-3-y player-position)
                                     (vector-3-z player-position)))
                                   ((is-key-down? keyboard-keys/key-down)
                                    (make-vector-3
                                     (vector-3-x player-position)
                                     (vector-3-y player-position)
                                     (+ (vector-3-z player-position) 0.2)))
                                   ((is-key-down? keyboard-keys/key-up)
                                    (make-vector-3
                                     (vector-3-x player-position)
                                     (vector-3-y player-position)
                                     (- (vector-3-z player-position) 0.2)))
                                   (else player-position)))
               (player-box
                (make-bounding-box (make-vector-3 (- (vector-3-x player-position)
                                                     (/ (vector-3-x player-size) 2))
                                                  (- (vector-3-y player-position)
                                                     (/ (vector-3-y player-size) 2))
                                                  (- (vector-3-z player-position)
                                                     (/ (vector-3-z player-size) 2)))
                                   (make-vector-3 (+ (vector-3-x player-position)
                                                     (/ (vector-3-x player-size) 2))
                                                  (+ (vector-3-y player-position)
                                                     (/ (vector-3-y player-size) 2))
                                                  (+ (vector-3-z player-position)
                                                     (/ (vector-3-z player-size) 2)))))
               (enemy-box
                (make-bounding-box (make-vector-3 (- (vector-3-x enemy-box-pos)
                                                     (/ (vector-3-x enemy-box-size) 2))
                                                  (- (vector-3-y enemy-box-pos)
                                                     (/ (vector-3-y enemy-box-size) 2))
                                                  (- (vector-3-z enemy-box-pos)
                                                     (/ (vector-3-z enemy-box-size) 2)))
                                   (make-vector-3 (+ (vector-3-x enemy-box-pos)
                                                     (/ (vector-3-x enemy-box-size) 2))
                                                  (+ (vector-3-y enemy-box-pos)
                                                     (/ (vector-3-y enemy-box-size) 2))
                                                  (+ (vector-3-z enemy-box-pos)
                                                     (/ (vector-3-z enemy-box-size) 2)))))
               (collision (or (check-collision-boxes player-box
                                                     enemy-box)
                              (check-collision-box-sphere player-box
                                                          enemy-sphere-pos
                                                          enemy-sphere-size)))
               (new-player-color (if collision
                                     RED
                                     GREEN)))

          (begin-drawing)

          (clear-background RAYWHITE)

          (begin-mode-3d cur-camera)

          ; Draw enemy-box
          (draw-cube enemy-box-pos
                     (vector-3-x enemy-box-size)
                     (vector-3-y enemy-box-size)
                     (vector-3-z enemy-box-size)
                     GRAY)
          (draw-cube-wires enemy-box-pos
                           (vector-3-x enemy-box-size)
                           (vector-3-y enemy-box-size)
                           (vector-3-z enemy-box-size)
                           DARKGRAY)

          ; Draw enemy-sphere
          (draw-sphere enemy-sphere-pos enemy-sphere-size GRAY)
          (draw-sphere-wires enemy-sphere-pos enemy-sphere-size 16 16 DARKGRAY)

          ; Draw player
          (draw-cube-v new-position player-size new-player-color)

          (draw-grid 10 10.0)

          (end-mode-3d)

          (draw-text "Move player with cursors to collide" 220 40 20 GRAY)

          (end-drawing)

          (main-loop (make-state player-position: new-position
                                 player-color: new-player-color)))
        (close-window)))
  (main-loop (make-state player-position: (make-vector-3 0.0 1.0 2.0)
                         player-color: GREEN)))
