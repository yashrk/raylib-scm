(include "raylib-definitions.scm")
(import defstruct
        raylib-scm)
(use format raylib-scm srfi-1)

(define screen-width 800)
(define screen-height 450)

(defstruct state
  frame-counter
  dropped-files)

(define (main-loop current-state)
  (if (not (window-should-close?))
      (let* ((dropped-files (state-dropped-files current-state))
             (dropped-files-count (length dropped-files)))
        (begin-drawing)
        (clear-background RAYWHITE)
        (if (eq? 0 dropped-files-count)
            (draw-text "Drop your files to this window!"
                       100
                       40
                       20
                       DARKGRAY)
            (begin
              (draw-text "Dropped files:"
                         100
                         40
                         20
                         DARKGRAY)
              (for-each (lambda (it)
                          (let* ((file (car it))
                                 (num (cadr it))
                                 (cur-color (if (eq? (modulo num 2) 0)
                                                (fade LIGHTGRAY 0.5)
                                                (fade LIGHTGRAY 0.3))))
                            (draw-rectangle 0 (+ (* num 40) 85) screen-width 40 cur-color)
                            (draw-text file 120 (+ (* num 40) 100) 10 GRAY)))
                        (zip dropped-files
                             (iota dropped-files-count)))
              (draw-text "Drop new files..."
                         100
                         (+ 110 (* dropped-files-count 40))
                         20
                         DARKGRAY)))
        (end-drawing)
        (main-loop (if (is-file-dropped)
                       (update-state current-state
                                     dropped-files: (get-dropped-files))
                       current-state)))
      (begin
        (clear-dropped-files)
        (close-window))))

(init-window screen-width screen-height "raylib [core] example - drop files")
(set-target-fps 60)

(main-loop (make-state dropped-files: '()))
