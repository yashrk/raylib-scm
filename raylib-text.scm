;;; Font Loading and Text Drawing Functions (Module: text)

(define draw-fps
  (foreign-lambda void "DrawFPS" int int))

(foreign-define-with-struct draw-text
                            "DrawText"
                            void
                            ((c-string text)
                             (int posX)
                             (int posY)
                             (int fontSize)
                             ((c-pointer (struct Color)) textColor)))

(foreign-define-with-struct measure-text
                            "MeasureText"
                            int
                            ((c-string text) (int fontSize)))

(foreign-define-with-struct draw-cube
                            "DrawCube"
                            void
                            (((c-pointer (struct Vector3)) position)
                             (float width)
                             (float height)
                             (float length)
                             ((c-pointer (struct Color)) color)))

(foreign-define-with-struct draw-cube-v
                            "DrawCubeV"
                            void
                            (((c-pointer (struct Vector3)) position)
                             ((c-pointer (struct Vector3)) size)
                             ((c-pointer (struct Color)) color)))

(foreign-define-with-struct draw-cube-wires
                            "DrawCubeWires"
                            void
                            (((c-pointer (struct Vector3)) position)
                             (float width)
                             (float height)
                             (float length)
                             ((c-pointer (struct Color)) color)))

(foreign-define-with-struct draw-sphere
                            "DrawSphere"
                            void
                            (((c-pointer (struct Vector3)) centerPos)
                             (float radius)
                             ((c-pointer (struct Color)) color)))

(foreign-define-with-struct draw-sphere-wires
                            "DrawSphereWires"
                            void
                            (((c-pointer (struct Vector3)) centerPos)
                             (float radius)
                             (int rings)
                             (int slices)
                             ((c-pointer (struct Color)) color)))

(foreign-define-with-struct draw-plane
                            "DrawPlane"
                            void
                            (((c-pointer (struct Vector3)) centerPos)
                             ((c-pointer (struct Vector2)) size)
                             ((c-pointer (struct Color)) color)))

(define draw-grid
  (foreign-lambda void "DrawGrid" int float))

(foreign-define-with-struct draw-gizmo
                            "DrawGizmo"
                            void
                            (((c-pointer (struct Vector3)) position)))

