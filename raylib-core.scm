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

(foreign-predicate is-file-dropped "IsFileDropped" int ())

(define get-dropped-files
  (foreign-safe-lambda* scheme-object ()
    "int count = 0;
     C_word lst = C_SCHEME_END_OF_LIST;
     C_word str_len, str;
     C_word* p;

     char** droppedFiles = GetDroppedFiles(&count);

     if (count == 0) {
       C_return(C_SCHEME_FALSE);
     }

     for (int i=0; i<count; i++) {
       str_len = strlen(droppedFiles[i]);
       p = C_alloc(C_SIZEOF_PAIR + C_SIZEOF_STRING(str_len));
       str = C_string(&p, str_len, droppedFiles[i]);
       lst = C_a_pair(&p, str, lst);
     }

     C_return(lst);"))

(define clear-dropped-files
  (foreign-lambda void "ClearDroppedFiles"))

;; Persistent storage management

;;; Input Handling Functions (Module: core)

(foreign-predicate is-key-pressed? "IsKeyPressed" int ((int keyCode)))

(foreign-predicate is-key-down? "IsKeyDown" int ((int keyCode)))

(foreign-predicate is-mouse-button-pressed? "IsMouseButtonPressed" int ((int mouseButton)))

(define get-mouse-x
  (foreign-lambda int "GetMouseX"))

(define get-mouse-y
  (foreign-lambda int "GetMouseY"))

(foreign-constructor get-mouse-position
                     "GetMousePosition"
                     vector-2
                     (c-pointer (struct Vector2)))

(define get-mouse-wheel-move
  (foreign-lambda int "GetMouseWheelMove"))
