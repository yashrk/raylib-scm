;;; Texture Loading and Drawing Functions (Module: textures)

(foreign-constructor load-image
                     "LoadImage"
                     image
                     (c-pointer (struct Image))
                     ((c-string fileName)))

(foreign-constructor load-texture
                     "LoadTexture"
                     texture-2d
                     (c-pointer (struct Texture2D))
                     ((c-string fileName)))

(foreign-constructor load-texture-from-image
                     "LoadTextureFromImage"
                     texture-2d
                     (c-pointer (struct Texture2D))
                     (((c-pointer (struct Image)) targetImage)))

(foreign-constructor load-render-texture
                     "LoadRenderTexture"
                     render-texture-2d
                     (c-pointer (struct RenderTexture2D))
                     ((int width)
                      (int height)))

(foreign-define-with-struct unload-image
                            "UnloadImage"
                            void
                            (((c-pointer (struct Image)) imageToUnload)))

(foreign-define-with-struct unload-texture
                            "UnloadTexture"
                            void
                            (((c-pointer (struct Texture2D)) textureToUnload)))

(foreign-define-with-struct unload-render-texture
                            "UnloadRenderTexture"
                            void
                            (((c-pointer (struct RenderTexture2D)) textureToUnload)))

;; Image manipulation functions

;; Image generation functions

;; Texture2D configuration functions

(define gen-texture-mipmaps
  (foreign-lambda void "GenTextureMipmaps" texture-2d))

;; Texture2D drawing functions

(foreign-define-with-struct draw-texture
                            "DrawTexture"
                            void
                            (((c-pointer (struct Texture2D)) texture)
                             (int posX)
                             (int posY)
                             ((c-pointer (struct Color)) tint)))

(foreign-define-with-struct draw-texture-rec
                            "DrawTextureRec"
                            void
                            (((c-pointer (struct Texture2D)) texture)
                             ((c-pointer (struct Rectangle)) sourceRec)
                             ((c-pointer (struct Vector2)) position)
                             ((c-pointer (struct Color)) tint)))

(foreign-define-with-struct draw-texture-pro
                            "DrawTexturePro"
                            void
                            (((c-pointer (struct Texture2D)) curTexture)
                             ((c-pointer (struct Rectangle)) sourceRec)
                             ((c-pointer (struct Rectangle)) destRec)
                             ((c-pointer (struct Vector2)) origin)
                             (float rotation)
                             ((c-pointer (struct Color)) tint)))

