;;; Window and Graphics Device Functions (Module: core)

;; Window-related functions

(define init-window
  (foreign-lambda void "InitWindow" int int c-string))

(foreign-predicate window-should-close? "WindowShouldClose" int ())

(define close-window
  (foreign-lambda void "CloseWindow"))

;; Drawing-related functions

(foreign-define-with-struct clear-background
                            "ClearBackground"
                            void
                            (((c-pointer (struct Color)) color)))

(define begin-drawing
  (foreign-lambda void "BeginDrawing"))

(define end-drawing
  (foreign-lambda void "EndDrawing"))

(foreign-define-with-struct begin-mode-3d
                            "BeginMode3D"
                            void
                            (((c-pointer (struct Camera3D)) cameraP)))

(define end-mode-3d
  (foreign-lambda void "EndMode3D"))

(foreign-define-with-struct begin-texture-mode
                            "BeginTextureMode"
                            void
                            (((c-pointer (struct RenderTexture2D)) target)))

(define end-texture-mode
  (foreign-lambda void "EndTextureMode"))

;; Screen-space-related functions

(foreign-constructor get-mouse-ray
                     "GetMouseRay"
                     ray
                     (c-pointer (struct Ray))
                     (((c-pointer (struct Vector2)) mousePosition)
                      ((c-pointer (struct Camera3D)) curCamera)))

;; timing-related functions

(define set-target-fps
  (foreign-lambda void "SetTargetFPS" int))

(define get-frame-time
  (foreign-lambda float "GetFrameTime"))

;; Color-related functions

(foreign-constructor fade
                     "Fade"
                     color
                     (c-pointer (struct Color))
                     (((c-pointer (struct Color)) baseColor)
                      (float alpha)))

;; Misc. functions

(define set-config-flags
  (foreign-lambda void "SetConfigFlags" unsigned-short))

(define set-trace-log-level
  (foreign-lambda void "SetTraceLogLevel" unsigned-short))

(define get-random-value
  (foreign-lambda int "GetRandomValue" int int))

;; Files management functions

;; Persistent storage management

;;; Input Handling Functions (Module: core)

(foreign-predicate is-key-pressed? "IsKeyPressed" int ((int keyCode)))

(foreign-predicate is-key-down? "IsKeyDown" int ((int keyCode)))

(foreign-predicate is-mouse-button-pressed? "IsMouseButtonPressed" int ((int mouseButton)))

(foreign-constructor get-mouse-position
                     "GetMousePosition"
                     vector-2
                     (c-pointer (struct Vector2)))

(define get-mouse-wheel-move
  (foreign-lambda int "GetMouseWheelMove"))
