;;; Camera System Functions (Module: camera)

(foreign-define-with-struct set-camera-mode
                            "SetCameraMode"
                            void
                            (((c-pointer (struct Camera3D)) targetCamera)
                             (int cameraMode)))

(define update-camera
  (foreign-lambda void "UpdateCamera" camera))


