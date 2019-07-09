;;; Constructors of raylib types

; Color
(define sizeof-color (foreign-value "sizeof(Color)" int))
(define-record rayc buffer)
(define-foreign-type rayc scheme-pointer rayc-buffer)
(define (make-color r g b a)
  (define set-fields!
    (foreign-lambda* void ((rayc c)
                           (unsigned-short r)
                           (unsigned-short g)
                           (unsigned-short b)
                           (unsigned-short a))
      "((Color*)c)->r = r;
       ((Color*)c)->g = g;
       ((Color*)c)->b = b;
       ((Color*)c)->a = a;"))
  (let ((c (make-rayc (make-blob sizeof-color))))
    (set-fields! c r g b a)
    c))

; Vector2
(define sizeof-vector-2 (foreign-value "sizeof(Vector2)" int))
(define-record v-2 buffer)
(define-foreign-type v-2 scheme-pointer v-2-buffer)
(define (make-vector-2 x y)
  (define set-fields!
    (foreign-lambda* void ((v-2 v) (float x) (float y))
      "((Vector2*)v)->x = x;
       ((Vector2*)v)->y = y;"))
  (let ((v (make-v-2 (make-blob sizeof-vector-2))))
    (set-fields! v x y)
    v))

;;; Window and Graphics Device Functions (Module: core)

;; Window-related functions

(define init-window
  (foreign-lambda void "InitWindow" int int c-string))

(foreign-predicate window-should-close? "WindowShouldClose" int ())

(define close-window
  (foreign-lambda void "CloseWindow"))

;; Drawing-related functions

(define (clear-background color)
  ((foreign-lambda* void ((rayc color))
     "ClearBackground(*((Color*)color));")
   color))

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

(define (fade base-color alpha)
  (let ((c (make-rayc (make-blob sizeof-color))))
    ((foreign-lambda* void ((rayc c) (rayc baseColor) (float alpha))
      "*((Color*)c) = Fade(*((Color*)baseColor), alpha);")
     c base-color alpha)
    c))

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

(foreign-predicate is-mouse-button-down? "IsMouseButtonDown" int ((int mouseButton)))

(foreign-predicate is-mouse-button-released? "IsMouseButtonReleased" int ((int mouseButton)))

(foreign-predicate is-mouse-button-up? "IsMouseButtonUp" int ((int mouseButton)))

(define get-mouse-x
  (foreign-lambda int "GetMouseX"))

(define get-mouse-y
  (foreign-lambda int "GetMouseY"))

(define (get-mouse-position)
  (let ((v (make-v-2 (make-blob sizeof-vector-2))))
    ((foreign-lambda* void ((v-2 v))
       "*((Vector2*)v) = GetMousePosition();")
     v)
    v))

(define get-mouse-wheel-move
  (foreign-lambda int "GetMouseWheelMove"))
