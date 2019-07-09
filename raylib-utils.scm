;; Data structure helper functions

(define (bounding-box-max box)
  (let ((new-vector (foreign-lambda* vector-3 (((c-pointer (struct BoundingBox)) box))
     "Vector3* vector = (Vector3*)malloc(sizeof(Vector3));
      *vector = box->max;
      C_return(vector);")))
    (set-finalizer! new-vector free)
    new-vector))

(define (bounding-box-min box)
  (let ((new-vector (foreign-lambda* vector-3 (((c-pointer (struct BoundingBox)) box))
     "Vector3* vector = (Vector3*)malloc(sizeof(Vector3));
      *vector = box->min;
      C_return(vector);")))
    (set-finalizer! new-vector free)
    new-vector))

(define (camera-position target-camera)
  (let ((position
         ((foreign-lambda* vector-3 (((c-pointer (struct Camera3D)) targetCamera))
            "Vector3* vector = (Vector3*)malloc(sizeof(Vector3));
             *vector = targetCamera->position;
             C_return(vector);")
          target-camera)))
    (set-finalizer! position free)
    position))

(define (camera-target target-camera)
  (let ((target
         ((foreign-lambda* vector-3 (((c-pointer (struct Camera3D)) targetCamera))
            "Vector3* vector = (Vector3*)malloc(sizeof(Vector3));
             *vector = targetCamera->target;
             C_return(vector);")
          target-camera)))
    (set-finalizer! target free)
    target))

(define (camera-up target-camera)
  (let ((up
         ((foreign-lambda* vector-3 (((c-pointer (struct Camera3D)) targetCamera))
            "Vector3* vector = (Vector3*)malloc(sizeof(Vector3));
             *vector = targetCamera->up;
             C_return(vector);")
          target-camera)))
    (set-finalizer! up free)
    up))

(define (camera-fovy target-camera)
  ((foreign-lambda* float (((c-pointer (struct Camera3D)) targetCamera))
     "C_return(targetCamera->fovy);")
   target-camera))

(define (camera-type target-camera)
  ((foreign-lambda* int (((c-pointer (struct Camera3D)) targetCamera))
     "C_return(targetCamera->type);")
   target-camera))

(define (color-r target-color)
  ((foreign-lambda* unsigned-short ((rayc color))
     "C_return(((Color*)color)->r);")
   target-color))

(define (color-g target-color)
  ((foreign-lambda* unsigned-short ((rayc color))
     "C_return(((Color*)color)->r);")
   target-color))

(define (color-b target-color)
  ((foreign-lambda* unsigned-short ((rayc color))
     "C_return(((Color*)color)->r);")
   target-color))

(define (color-a target-color)
  ((foreign-lambda* unsigned-short ((rayc color))
     "C_return(((Color*)color)->r);")
   target-color))

(define (get-diffuse-texture cur-model material-index)
  ((foreign-lambda* texture-2d (((c-pointer (struct Model)) curModel)
                                (int index))
     "C_return(&curModel->materials[index].maps[MAP_DIFFUSE].texture);")
   cur-model material-index))

(foreign-constructor* make-bounding-box
                      bounding-box
                      "BoundingBox"
                      (((c-pointer (struct Vector3)) min)
                       ((c-pointer (struct Vector3)) max)))

(foreign-constructor* make-camera
                      camera
                      "Camera3D"
                      (((c-pointer (struct Vector3)) position)
                       ((c-pointer (struct Vector3)) target)
                       ((c-pointer (struct Vector3)) up)
                       (float fovy)
                       (int type)))

(foreign-constructor* make-ray
                      bounding-box
                      "Ray"
                      (((c-pointer (struct Vector3)) position)
                       ((c-pointer (struct Vector3)) direction)))

(foreign-constructor* make-rectangle
                      rectangle
                      "Rectangle"
                      ((float x)
                       (float y)
                       (float width)
                       (float height)))

(foreign-constructor* make-render-texture-2d
                      render-texture-2d
                      "RenderTexture2D"
                      ((unsigned-int id)
                       ((c-pointer (struct Texture2D)) texture)
                       ((c-pointer (struct Texture2D)) depth)))

(foreign-constructor* make-vector-3
                      vector-3
                      "Vector3"
                      ((float x)
                       (float y)
                       (float z)))

(define (model-mesh target-model mesh-index)
  (let ((new-mesh
         ((foreign-lambda* mesh (((c-pointer (struct Model)) model)
                                 (int index))
            "Mesh* mesh = (Mesh*)malloc(sizeof(Mesh));
             *mesh = model->meshes[index];
             C_return(mesh);")
          target-model mesh-index)))
    (set-finalizer! new-mesh free)
    new-mesh))

(define (model-transform target-model)
  (let ((new-matrix
         ((foreign-lambda* matrix (((c-pointer (struct Model)) model))
            "Matrix* matrix = (Matrix*)malloc(sizeof(Matrix));
             *matrix = model->transform;
             C_return(matrix);")
          target-model)))
    (set-finalizer! new-matrix free)
    new-matrix))

(define (model-material target-model material-index)
  (let ((new-material
         ((foreign-lambda* material (((c-pointer (struct Model)) model)
                                     (int index))
            "Material* material = (Material*)malloc(sizeof(Material));
             *material = model->materials[index];
             C_return(material);")
          target-model material-index)))
    (set-finalizer! new-material free)
    new-material))

(define (rectangle-x rec)
  ((foreign-lambda* float (((c-pointer (struct Rectangle)) rec))
     "C_return(rec->x);")
   rec))

(define (rectangle-y rec)
  ((foreign-lambda* float (((c-pointer (struct Rectangle)) rec))
     "C_return(rec->y);")
   rec))

(define (rectangle-width rec)
  ((foreign-lambda* float (((c-pointer (struct Rectangle)) rec))
     "C_return(rec->width);")
   rec))

(define (rectangle-height rec)
  ((foreign-lambda* float (((c-pointer (struct Rectangle)) rec))
     "C_return(rec->height);")
   rec))

(define (render-texture-texture target-texture)
  ((foreign-lambda* texture-2d (((c-pointer (struct RenderTexture2D)) targetTexture))
    "C_return(&(targetTexture->texture));")
   target-texture))

(define (set-cubemap-texture source-model material-index new-texture)
  (let ((result ((foreign-lambda* model (((c-pointer (struct Model)) sourceModel)
                                         (int index)
                                         ((c-pointer (struct Texture2D)) newTexture))
                   "Model* model = (Model*)malloc(sizeof(Model));
                    *model = *sourceModel;
                    model->materials[index].maps[MAP_CUBEMAP].texture = *newTexture;
                    C_return(model);")
                 source-model material-index new-texture)))
    (set-finalizer! result free)
    result))

(define (set-diffuse-texture source-model material-index new-texture)
  (let ((result ((foreign-lambda* model (((c-pointer (struct Model)) sourceModel)
                                         (int index)
                                         ((c-pointer (struct Texture2D)) newTexture))
                   "Model* model = (Model*)malloc(sizeof(Model));
                    *model = *sourceModel;
                    model->materials[index].maps[MAP_DIFFUSE].texture = *newTexture;
                    C_return(model);")
                 source-model material-index new-texture)))
    (set-finalizer! result free)
    result))

(define (set-material-shader! target-model material-index material-shader)
  ((foreign-lambda* void (((c-pointer (struct Model)) targetModel)
                          (int index)
                          ((c-pointer (struct Shader)) materialShader))
     "targetModel->materials[index].shader = *materialShader;")
   target-model material-index material-shader))

(define (set-transform target-model transform)
  (let ((result ((foreign-lambda* model (((c-pointer (struct Model)) targetModel)
                                         ((c-pointer (struct Matrix)) transform))
                   "Model* model = (Model*)malloc(sizeof(Model));
                    *model = *targetModel;
                    model->transform = *transform;
                    C_return(model);")
                 target-model transform)))
    (set-finalizer! result free)
    result))

(define (set-transform! target-model transform)
  ((foreign-lambda* void (((c-pointer (struct Model)) targetModel)
                          ((c-pointer (struct Matrix)) transform))
     "targetModel->transform = *transform;")
   target-model transform))

(define (texture-2d-width texture)
  ((foreign-lambda* int (((c-pointer (struct Texture2D)) texture))
     "C_return(texture->width);")
   texture))

(define (texture-2d-height texture)
  ((foreign-lambda* int (((c-pointer (struct Texture2D)) texture))
     "C_return(texture->height);")
   texture))

(define vector-2-x
  (foreign-lambda* float ((v-2 v))
    "C_return(((Vector2*)v)->x);"))

(define vector-2-y
  (foreign-lambda* float ((v-2 v))
    "C_return(((Vector2*)v)->y);"))

(define (vector-3-x vector)
  ((foreign-lambda* float (((c-pointer (struct Vector3)) vector))
     "C_return(vector->x);")
   vector))

(define (vector-3-y vector)
  ((foreign-lambda* float (((c-pointer (struct Vector3)) vector))
     "C_return(vector->y);")
   vector))

(define (vector-3-z vector)
  ((foreign-lambda* float (((c-pointer (struct Vector3)) vector))
     "C_return(vector->z);")
   vector))

;; Utilities

(define (allocate-float number)
  (let ((new-pointer
         ((foreign-lambda* (c-pointer float) ((float number))
            "float* newPointer = (float*)malloc(sizeof(float));
             *newPointer = number;
             C_return(newPointer);")
          number)))
    (set-finalizer! new-pointer free)
    new-pointer))

(define (allocate-int number)
  (let ((new-pointer
         ((foreign-lambda* (c-pointer int) ((int number))
            "int* newPointer = (int*)malloc(sizeof(int));
             *newPointer = number;
             C_return(newPointer);")
          number)))
    (set-finalizer! new-pointer free)
    new-pointer))

(define (deg->rad deg-angle)
  ((foreign-lambda* float ((float degAngle))
     "C_return(DEG2RAD * degAngle);")
   deg-angle))

(define (rad->deg rad-angle)
  ((foreign-lambda* float ((float radAngle))
     "C_return(RAD2DEG * radAngle);")
   rad-angle))

(define LIGHTGRAY  (make-color 200 200 200 255))   ; Light Gray
(define GRAY       (make-color 130 130 130 255))   ; Gray
(define DARKGRAY   (make-color 80 80 80 255))      ; Dark Gray
(define YELLOW     (make-color 253 249 0 255))     ; Yellow
(define GOLD       (make-color 255 203 0 255))     ; Gold
(define ORANGE     (make-color 255 161 0 255))     ; Orange
(define PINK       (make-color 255 109 194 255))   ; Pink
(define RED        (make-color 230 41 55 255))     ; Red
(define MAROON     (make-color 190 33 55 255))     ; Maroon
(define GREEN      (make-color 0 228 48 255))      ; Green
(define LIME       (make-color 0 158 47 255))      ; Lime
(define DARKGREEN  (make-color 0 117 44 255))      ; Dark Green
(define SKYBLUE    (make-color 102 191 255 255))   ; Sky Blue
(define BLUE       (make-color 0 121 241 255))     ; Blue
(define DARKBLUE   (make-color 0 82 172 255))      ; Dark Blue
(define PURPLE     (make-color 200 122 255 255))   ; Purple
(define VIOLET     (make-color 135 60 190 255))    ; Violet
(define DARKPURPLE (make-color 112 31 126 255))    ; Dark Purple
(define BEIGE      (make-color 211 176 131 255))   ; Beige
(define BROWN      (make-color 127 106 79 255))    ; Brown
(define DARKBROWN  (make-color 76 63 47 255))      ; Dark Brown

(define WHITE      (make-color 255 255 255 255))   ; White
(define BLACK      (make-color 0 0 0 255))         ; Black
(define BLANK      (make-color 0 0 0 0))           ; Blank (Transparent)
(define MAGENTA    (make-color 255 0 255 255))     ; Magenta
(define RAYWHITE   (make-color 245 245 245 255))   ; My own White (raylib logo)
