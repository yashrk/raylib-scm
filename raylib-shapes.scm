;;; Basic Shapes Drawing Functions (Module: shapes)

(foreign-define-with-struct draw-circle-v
                            "DrawCircleV"
                            void
                            (((c-pointer (struct Vector2)) center)
                             (float radius)
                             ((c-pointer (struct Color)) color)))

(foreign-define-with-struct draw-rectangle
                            "DrawRectangle"
                            void
                            ((int posX)
                             (int posY)
                             (int width)
                             (int height)
                             ((c-pointer (struct Color)) color)))

(foreign-define-with-struct draw-rectangle-rec
                            "DrawRectangleRec"
                            void
                            (((c-pointer (struct Rectangle)) rec)
                             ((c-pointer (struct Color)) color)))

(foreign-define-with-struct draw-rectangle-lines
                            "DrawRectangleLines"
                            void
                            ((int posX)
                             (int posY)
                             (int width)
                             (int height)
                             ((c-pointer (struct Color)) color)))

;;; Basic shapes collision detection functions

(foreign-predicate check-collision-point-rec
                   "CheckCollisionPointRec"
                   int
                   (((c-pointer (struct Vector2)) point)
                    ((c-pointer (struct Rectangle)) rec)))

