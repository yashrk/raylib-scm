(include "raylib-definitions.scm")
(import raylib-scm)
(use raylib-scm)

(define screen-width 800)
(define screen-height 450)

(set-trace-log-level trace-log-type/log-debug)

(init-window screen-width screen-height "raylib [audio] example - sound loading and playing")

(init-audio-device)                                              ; Initialize audio device
(set-target-fps 60)

(let ((fx-wav (load-sound "resources/sound.wav"))                ; Load WAV audio file
      (fx-ogg (load-sound "resources/tanatana.ogg")))            ; Load OGG audio file

    (define (main-loop)
      (if (not (window-should-close?))
          (begin

            (cond ((is-key-pressed? keyboard-keys/key-space)
                   (play-sound fx-wav))                          ; Play WAV sound
                  ((is-key-pressed? keyboard-keys/key-enter)     ; Play OGG sound
                   (play-sound fx-ogg)))

            (begin-drawing)
            (clear-background RAYWHITE)
            (draw-text "Press SPACE to PLAY the WAV sound!" 200 180 20 LIGHTGRAY)
            (draw-text "Press ENTER to PLAY the OGG sound!" 200 220 20 LIGHTGRAY)
            (end-drawing)
            (main-loop))
          (begin
            (unload-sound fx-wav)
            (unload-sound fx-ogg)
            (close-audio-device)
            (close-window))))

    (main-loop))
