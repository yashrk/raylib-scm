;;; Shaders System Functions (Module: rlgl)
;;; NOTE: This functions are useless when using OpenGL 1.1

;; Shader loading/unloading functions

(foreign-constructor load-shader
                     "LoadShader"
                     shader
                     (c-pointer (struct Shader))
                     ((c-string vsFileName)
                      (c-string fsFileName)))

(foreign-define-with-struct unload-shader
                            "UnloadShader"
                            void
                            (((c-pointer (struct Shader)) shaderToUnload)))

;; Shader configuration functions

(foreign-define-with-struct get-shader-location
                            "GetShaderLocation"
                            int
                            (((c-pointer (struct Shader)) targetShader)
                             (c-string uniformName)))

(foreign-define-with-struct set-shader-value!
                            "SetShaderValue"
                            void
                            (((c-pointer (struct Shader)) shader)
                             (int uniformLoc)
                             ((c-pointer void) value)
                             (int uniformType)))

;; Texture maps generation (PBR)
;; NOTE: Required shaders should be provided

(foreign-constructor gen-texture-cubemap
                     "GenTextureCubemap"
                     texture-2d
                     (c-pointer (struct Texture2D))
                     (((c-pointer (struct Shader)) shader)
                      ((c-pointer (struct Texture2D)) skyHDR)
                      (int size)))

;; Shading begin/end functions

(foreign-define-with-struct begin-shader-mode
                            "BeginShaderMode"
                            void
                            (((c-pointer (struct Shader)) shader)))

(define end-shader-mode
  (foreign-lambda void "EndShaderMode"))

(define begin-blend-mode
  (foreign-lambda void "BeginBlendMode" int))

(define end-blend-mode
  (foreign-lambda void "EndBlendMode"))

