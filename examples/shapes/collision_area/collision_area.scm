(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use format
     raylib-scm)

(define screen-width 800)
(define screen-height 450)
(define screen-upper-limit 40)

(init-window screen-width screen-height "raylib [shapes] example - collision area")
(set-target-fps 60)

(defstruct state
  box-a
  box-a-speed-x
  box-b
  box-collision
  pause
  collision)

(define (main-loop current-state)
  (if (not (window-should-close?))
      (let* ((box-a (state-box-a current-state))
             (box-a-speed-x (state-box-a-speed-x current-state))
             (box-b (state-box-b current-state))
             (box-collision (state-box-collision current-state))
             (pause (state-pause current-state))
             (collision (state-collision current-state))
             ; Update player-controlled-box (box02)
             (box-b (make-rectangle (- (get-mouse-x)
                                       (/ (rectangle-width box-b) 2))
                                    (- (get-mouse-y)
                                       (/ (rectangle-height box-b) 2))
                                    (rectangle-width box-b)
                                    (rectangle-height box-b)))
             ; Bounce box on x screen limits
             (box-a-speed-x (if (or (>= (+ (rectangle-x box-a)
                                           (rectangle-width box-a))
                                        screen-width)
                                    (< (rectangle-x box-a)
                                       0))
                                (* box-a-speed-x -1)
                                box-a-speed-x)))

        (begin-drawing)

        (clear-background RAYWHITE)
        (draw-rectangle 0 0 screen-width screen-upper-limit (if collision RED BLACK))

        (draw-rectangle-rec box-a GOLD)
        (draw-rectangle-rec box-b BLUE)

        (when collision
          ; Draw collision area
          (draw-rectangle-rec box-collision LIME)

          ; Draw collision message
          (draw-text "COLLISION!"
                     (- (/ screen-width 2) (/ (measure-text "COLLISION!" 20) 2))
                     (- (/ screen-upper-limit 2) 10)
                     20
                     BLACK)

          ; Draw collision area
          (draw-text (format #f
                             "Collision Area: ~d"
                             (inexact->exact
                              (* (rectangle-width box-collision) (rectangle-height box-collision))))
                     (- (/ screen-width 2) 100)
                     (+ screen-upper-limit 10)
                     20
                     BLACK))

        (draw-fps 10 10)

        (end-drawing)

        (main-loop (make-state
                    ; Move box if not paused
                    box-a: (if pause
                               box-a
                               (make-rectangle (+ (rectangle-x box-a) box-a-speed-x)
                                               (rectangle-y box-a)
                                               (rectangle-width box-a)
                                               (rectangle-height box-a)))
                    box-a-speed-x: box-a-speed-x
                    ; Make sure Box B does not go out of move area limits
                    box-b: (make-rectangle
                            (cond ((>= (+ (rectangle-x box-b) (rectangle-width box-b))
                                       screen-width)
                                   (- screen-width (rectangle-width box-b)))
                                  ((< (rectangle-x box-b) 0)
                                   0)
                                  (else
                                   (rectangle-x box-b)))
                            (cond ((>= (+ (rectangle-y box-b) (rectangle-height box-b))
                                       screen-height)
                                   (- screen-height (rectangle-height box-b)))
                                  ((< (rectangle-y box-b) 0)
                                   0)
                                  (else
                                   (rectangle-y box-b)))
                            (rectangle-width box-b)
                            (rectangle-height box-b))
                    ; Get collision rectangle (only on collision)
                    box-collision: (if collision
                                       (get-collision-rec box-a box-b)
                                       (make-rectangle 0 0 0 0))
                    ; Pause Box A movement
                    pause: (if (is-key-pressed? keyboard-keys/key-space)
                               (not pause)
                               pause)
                    ; Check boxes collision
                    collision: (check-collision-recs box-a box-b))))
      (close-window)))

(main-loop (make-state box-a: (make-rectangle 10
                                              (- (/ screen-height 2) 50)
                                              200
                                              100)
                       box-a-speed-x: 4
                       box-b: (make-rectangle (- (/ screen-width 2) 30)
                                              (- (/ screen-height 2) 30)
                                              60
                                              60)
                       box-collision: (make-rectangle 0 0 0 0)
                       pause: #f
                       collision: #f))
