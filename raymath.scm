;; raymath

(foreign-constructor* make-matrix
                      matrix
                      "Matrix"
                      ((float m0) (float m4) (float m8)  (float m12)
                       (float m1) (float m5) (float m9)  (float m13)
                       (float m2) (float m6) (float m10) (float m14)
                       (float m3) (float m7) (float m11) (float m15)))

(foreign-constructor matrix-identity
                     "MatrixIdentity"
                     matrix
                     (c-pointer (struct Matrix)))

(foreign-constructor matrix-invert
                     "MatrixInvert"
                     matrix
                     (c-pointer (struct Matrix))
                     (((c-pointer (struct Matrix)) targetMatrix)))

(foreign-constructor matrix-multiply
                     "MatrixMultiply"
                     matrix
                     (c-pointer (struct Matrix))
                     (((c-pointer (struct Matrix)) left)
                      ((c-pointer (struct Matrix)) right)))

(foreign-constructor matrix-rotate
                     "MatrixRotate"
                     matrix
                     (c-pointer (struct Matrix))
                     (((c-pointer (struct Vector3)) axis)
                      (float angle)))

(foreign-constructor matrix-rotate-x
                     "MatrixRotateX"
                     matrix
                     (c-pointer (struct Matrix))
                     ((float angle)))

(foreign-constructor matrix-rotate-y
                     "MatrixRotateY"
                     matrix
                     (c-pointer (struct Matrix))
                     ((float angle)))

(foreign-constructor matrix-rotate-z
                     "MatrixRotateZ"
                     matrix
                     (c-pointer (struct Matrix))
                     ((float angle)))

(foreign-constructor vector-3-add
                     "Vector3Add"
                     vector-3
                     (c-pointer (struct Vector3))
                     (((c-pointer (struct Vector3)) vector1)
                      ((c-pointer (struct Vector3)) vector2)))

(foreign-constructor vector-3-negate
                     "Vector3Negate"
                     vector-3
                     (c-pointer (struct Vector3))
                     (((c-pointer (struct Vector3)) vector)))

(foreign-constructor vector-3-scale
                     "Vector3Scale"
                     vector-3
                     (c-pointer (struct Vector3))
                     (((c-pointer (struct Vector3)) vector)
                      (float scale)))

(foreign-constructor vector-3-substract
                     "Vector3Subtract"
                     vector-3
                     (c-pointer (struct Vector3))
                     (((c-pointer (struct Vector3)) vector1)
                      ((c-pointer (struct Vector3)) vector2)))

(foreign-constructor vector-3-transform
                     "Vector3Transform"
                     vector-3
                     (c-pointer (struct Vector3))
                     (((c-pointer (struct Vector3)) vector)
                      ((c-pointer (struct Matrix)) matrix)))

