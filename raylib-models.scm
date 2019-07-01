;;; Model 3d Loading and Drawing Functions (Module: models)

;; Model loading/unloading functions

(foreign-constructor load-model
                     "LoadModel"
                     model
                     (c-pointer (struct Model))
                     ((c-string fileName)))

(foreign-constructor load-model-from-mesh
                     "LoadModelFromMesh"
                     model
                     (c-pointer (struct Model))
                     (((c-pointer (struct Mesh)) sourceMesh)))

(foreign-define-with-struct unload-model
                            "UnloadModel"
                            void
                            (((c-pointer (struct Model)) modelToUnload)))

;; Mesh generation functions

(foreign-constructor gen-mesh-cube
                     "GenMeshCube"
                     mesh
                     (c-pointer (struct Mesh))
                     ((float width)
                      (float height)
                      (float length)))

(foreign-constructor gen-mesh-heightmap
                     "GenMeshHeightmap"
                     mesh
                     (c-pointer (struct Mesh))
                     (((c-pointer (struct Image)) mapImage)
                      ((c-pointer (struct Vector3)) sizeVector)))

;; Material loading/unloading functions

;; Model drawing functions

(foreign-define-with-struct draw-model
                            "DrawModel"
                            void
                            (((c-pointer (struct Model)) model)
                             ((c-pointer (struct Vector3)) position)
                             (float scale)
                             ((c-pointer (struct Color)) tint)))

(foreign-define-with-struct draw-billboard
                            "DrawBillboard"
                            void
                            (((c-pointer (struct Camera3D)) camera)
                             ((c-pointer (struct Texture2D)) texture)
                             ((c-pointer (struct Vector3)) center)
                             (float size)
                             ((c-pointer (struct Color)) tint)))

;; Collision detection functions

(foreign-predicate check-collision-boxes
                   "CheckCollisionBoxes"
                   int
                   (((c-pointer (struct BoundingBox)) box1)
                    ((c-pointer (struct BoundingBox)) box2)))

(foreign-predicate check-collision-box-sphere
                   "CheckCollisionBoxSphere"
                   int
                   (((c-pointer (struct BoundingBox)) box)
                    ((c-pointer (struct Vector3)) centerSphere)
                    (float radiusSphere)))

(foreign-predicate check-collision-ray-box
                   "CheckCollisionRayBox"
                   int
                   (((c-pointer (struct Ray)) ray)
                    ((c-pointer (struct BoundingBox)) box)))

