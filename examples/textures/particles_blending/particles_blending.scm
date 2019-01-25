(include "raylib-definitions.scm")
(import defstruct
        raylib-scm
        vlist)
(use defstruct
     raylib-scm
     srfi-1
     vlist)

(define screen-width 800)
(define screen-height 450)

(define max-particles 200)

(define gravity 3.0)

(defstruct state
  mouse-tail ; Particles pool
  blend-mode)

; Particle structure with basic data
(defstruct particle
  position
  color
  alpha
  size
  rotation
  active) ; NOTE: Use it to activate/deactive particle

(define (vlist-findp l predicate)
  (define (vlist-findp-internal l i)
    (cond ((vlist-null? l) i)
          ((predicate (vlist-head l)) i)
          ((vlist-findp-internal (vlist-tail l) (+ i 1)))))
  (vlist-findp-internal l 0))

(define (vlist-replace-nth l index value)
  (vlist-append (vlist-take l index)
                (list->vlist (list value))
                (vlist-drop l (+ index 1))))

(init-window screen-width screen-height "raylib [textures] example - particles blending")

(let* ((smoke (load-texture "resources/smoke.png"))
       (smoke-width (texture-2d-width smoke))
       (smoke-height (texture-2d-height smoke))
       ; Initialize particles
       (particles (list->vlist
                   (map (lambda (_)
                          (make-particle
                           position: (make-vector-2 0.0 0.0)
                           color: (make-color (get-random-value 0 255)
                                              (get-random-value 0 255)
                                              (get-random-value 0 255)
                                              (get-random-value 0 255))
                           alpha: 1.0
                           size: (/ (get-random-value 1 30) 20)
                           rotation: (get-random-value 1 360)
                           active: #f))
                        (iota max-particles)))))

  (define (main-loop cur-state)
    (if (not (window-should-close?))
        (let* ((mouse-tail (state-mouse-tail cur-state))
               (blending (state-blend-mode cur-state))
               (first-not-active (vlist-findp mouse-tail
                                              (lambda (p)
                                                (not (particle-active p)))))
               (mouse-tail
                (if (< first-not-active max-particles)
                    (vlist-replace-nth
                     mouse-tail
                     first-not-active
                     (make-particle position: (get-mouse-position)
                                    color: (particle-color (vlist-ref mouse-tail
                                                                      first-not-active))
                                    alpha: 1.0
                                    size: (particle-size (vlist-ref mouse-tail
                                                                    first-not-active))
                                    rotation: (particle-rotation (vlist-ref mouse-tail
                                                                            first-not-active))
                                    active: #t))
                    mouse-tail)))

          (begin-drawing)

          (clear-background DARKGRAY)

          (begin-blend-mode blending)
          (vlist-for-each (lambda (p)
                            (when (particle-active p)
                              (draw-texture-pro smoke
                                                (make-rectangle 0.0
                                                                0.0
                                                                smoke-width
                                                                smoke-height)
                                                (make-rectangle (vector-2-x (particle-position p))
                                                                (vector-2-y (particle-position p))
                                                                (* (particle-size p)
                                                                   smoke-width)
                                                                (* (particle-size p)
                                                                   smoke-height))
                                                (make-vector-2 (* smoke-width
                                                                  (/ (particle-size p) 2))
                                                               (* smoke-height
                                                                  (/ (particle-size p) 2)))
                                                (particle-rotation p)
                                                (fade (particle-color p)
                                                      (particle-alpha p)))))
                          mouse-tail)
          (end-blend-mode)

          (draw-text "PRESS SPACE to CHANGE BLENDING MODE" 180 20 20 BLACK)

          (if (eq? blending blend-mode/blend-alpha)
              (draw-text "ALPHA BLENDING" 290 (- screen-height 40) 20 BLACK)
              (draw-text "ADDITIVE BLENDING" 280 (- screen-height 40) 20 RAYWHITE))

          (end-drawing)

          (main-loop (make-state
                      mouse-tail: (vlist-map
                                   (lambda (p)
                                     (if (particle-active p)
                                         (make-particle position: (make-vector-2
                                                                   (vector-2-x
                                                                    (particle-position p))
                                                                   (+ (vector-2-y
                                                                       (particle-position p))
                                                                      gravity))
                                                        color: (particle-color p)
                                                        alpha: (- (particle-alpha p) 0.01)
                                                        size: (particle-size p)
                                                        rotation: (+ (particle-rotation p) 5.0)
                                                        active: (> (particle-alpha p) 0.0))
                                         p))
                                   mouse-tail)
                      blend-mode: (if (is-key-pressed? keyboard-keys/key-space)
                                      (if (eq? (state-blend-mode cur-state) blend-mode/blend-alpha)
                                          blend-mode/blend-additive
                                          blend-mode/blend-alpha)
                                      (state-blend-mode cur-state)))))
        (begin
          (close-window))))

  (set-target-fps 60)

  (main-loop (make-state mouse-tail: particles
                         blend-mode: blend-mode/blend-alpha)))
