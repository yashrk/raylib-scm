;;; Raylib (https://www.raylib.com/) bindings
;;; License: GNU Affero General Public License (https://www.gnu.org/licenses/agpl-3.0.en.html)

(foreign-declare "#include <raylib.h>
                  #include \"raymath.h\"")

(include "raylib-definitions.scm")

(module raylib-scm
    (;;; Window and Graphics Device Functions (Module: core)

     ;; Window-related functions

     init-window                                           ; Initialize window and OpenGL context
     window-should-close?                                  ; Check if KEY_ESCAPE pressed or
                                                           ; Close icon pressed
     close-window                                          ; Close window and unload OpenGL context
     ;; is-window-ready                                       ; Check if window has been initialized successfully
     ;; is-window-minimized                                   ; Check if window has been minimized (or lost focus)
     ;; is-window-hidden                                      ; Check if window is currently hidden
     ;; toggle-fullscreen                                     ; Toggle fullscreen mode (only PLATFORM_DESKTOP)
     ;; unhide-window                                         ; Show the window
     ;; hide-window                                           ; Hide the window
     ;; set-window-icon                                       ; Set icon for window (only PLATFORM_DESKTOP)
     ;; set-window-title                                      ; Set title for window (only PLATFORM_DESKTOP)
     ;; set-window-position                                   ; Set window position on screen (only PLATFORM_DESKTOP)
     ;; set-window-monitor                                    ; Set monitor for the current window (fullscreen mode)
     ;; set-window-min-size                                   ; Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
     ;; set-window-size                                       ; Set window dimensions
     ;; get-window-handle                                     ; Get native window handle
     ;; get-screen-width                                      ; Get current screen width
     ;; get-screen-height                                     ; Get current screen height
     ;; get-monitor-count                                     ; Get number of connected monitors
     ;; get-monitor-width                                     ; Get primary monitor width
     ;; get-monitor-height                                    ; Get primary monitor height
     ;; get-monitor-physical-width                            ; Get primary monitor physical width in millimetres
     ;; get-monitor-physical-height                           ; Get primary monitor physical height in millimetres
     ;; get-monitor-name                                      ; Get the human-readable, UTF-8 encoded name of the primary monitor

     ;; Cursor-related functions

     ;; show-cursor                                           ; Shows cursor
     ;; hide-cursor                                           ; Hides cursor
     ;; is-cursor-hidden                                      ; Check if cursor is not visible
     ;; enable-cursor                                         ; Enables cursor (unlock cursor)
     ;; disable-cursor                                        ; Disables cursor (lock cursor)

     ;; Drawing-related functions

     clear-background                                      ; Set background color
                                                           ; (framebuffer clear color)
     begin-drawing                                         ; Setup canvas (framebuffer) to start drawing
     end-drawing                                           ; End canvas drawing and swap buffers (double buffering)
     ;; begin-mode-2d                                         ; Initialize 2D mode with custom camera (2D)
     ;; end-mode-2d                                           ; Ends 2D mode with custom camera
     begin-mode-3d                                         ; Initializes 3D mode with custom camera (3D)
     end-mode-3d                                           ; Ends 3D mode and returns to default 2D orthographic mode
     begin-texture-mode                                    ; Initializes render texture for drawing
     end-texture-mode                                      ; Ends drawing to render texture

     ;; Screen-space-related functions

     get-mouse-ray                                         ; Returns a ray trace from mouse position
     ;; get-world-to-screen                                   ; Returns the screen space position for a 3d world space position
     ;; get-camera-matrix                                     ; Returns camera transform matrix (view matrix)

     ;; timing-related functions

     set-target-fps                                        ; Set target FPS (maximum)
     ;; get-fps                                               ; Returns current FPS
     get-frame-time                                        ; Returns time in seconds for last frame drawn
     ;; get-time                                              ; Returns elapsed time in seconds since InitWindow()

     ;; Color-related functions

     ;; color-to-int                                          ; Returns hexadecimal value for a Color
     ;; color-normalize                                       ; Returns color normalized as float [0..1]
     ;; color-to-hsv                                          ; Returns HSV values for a Color
     ;; color-from-hsv                                        ; Returns a Color from HSV values
     ;; get-color                                             ; Returns a Color struct from hexadecimal value
     fade                                                  ; Color fade-in or fade-out,
                                                           ; alpha goes from 0.0f to 1.0f

     ;; Misc. functions

     set-config-flags                                      ; Setup window configuration flags (view FLAGS)
     set-trace-log-level                                   ; Set the current threshold (minimum)
                                                           ; log level
     ;; set-trace-log-callback                                ; Set a trace log callback to enable custom logging bypassing raylib's one
     ;; trace-log                                             ; Show trace log messages (LOG_INFO, LOG_WARNING, LOG_ERROR, LOG_DEBUG)
     ;; take-screenshot                                       ; Takes a screenshot of current screen (saved a .png)
     get-random-value                                      ; Returns a random value between min and max (both included)

     ;; Files management functions

     ;; file-exists                                           ; Check if file exists
     ;; is-file-extension-                                    ; Check file extension
     ;; get-extension                                         ; Get pointer to extension for a filename string
     ;; get-file-name                                         ; Get pointer to filename for a path string
     ;; get-file-name-without-ext                             ; Get filename string without extension (memory should be freed)
     ;; get-directory-path                                    ; Get full path for a given fileName (uses static string)
     ;; get-working-directory                                 ; Get current working directory (uses static string)
     ;; get-directory-files-                                  ; Get filenames in a directory path (memory should be freed)
     ;; clear-directory-files                                 ; Clear directory files paths buffers (free memory)
     ;; change-directory                                      ; Change working directory, returns true if success
     ;; is-file-dropped                                       ; Check if a file has been dropped into window
     ;; get-dropped-files                                     ; Get dropped files names (memory should be freed)
     ;; clear-dropped-files                                   ; Clear dropped files paths buffer (free memory)
     ;; get-file-mod-time                                     ; Get file modification time (last write time)

     ;; ; Persistent storage management
     ;; StorageSaveValue                                      ; Save integer value to storage file (to defined position)
     ;; StorageLoadValue                                      ; Load integer value from storage file (from defined position)

     ;; open-url                                              ; Open URL with default system browser (if available)

     ;;; Input Handling Functions (Module: core)

     ;; Input-related functions: keyboard

     is-key-pressed?                                       ; Detect if a key has been pressed once
     is-key-down?                                          ; Detect if a key is being pressed
     ;; is-key-released?                                      ; Detect if a key has been released once
     ;; is-key-up?                                            ; Detect if a key is NOT being pressed
     ;; get-key-pressed                                       ; Get latest key pressed
     ;; set-exit-key                                          ; Set a custom key to exit program (default is ESC)

     ;; Input-related functions: gamepads

     ;; is-gamepad-available?                                 ; Detect if a gamepad is available
     ;; is-gamepad-name?                                      ; Check gamepad name (if available)
     ;; get-gamepad-name                                      ; Return gamepad internal name id
     ;; is-gamepad-button-pressed?                            ; Detect if a gamepad button has been pressed once
     ;; is-gamepad-button-down?                               ; Detect if a gamepad button is being pressed
     ;; is-gamepad-button-released?                           ; Detect if a gamepad button has been released once
     ;; is-gamepad-button-up?                                 ; Detect if a gamepad button is NOT being pressed
     ;; get-gamepad-button-pressed                            ; Get the last gamepad button pressed
     ;; get-gamepad-axis-count                                ; Return gamepad axis count for a gamepad
     ;; get-gamepad-axis-movement                             ; Return axis movement value for a gamepad axis

     ;; Input-related functions: mouse

     is-mouse-button-pressed?                              ; Detect if a mouse button has been
                                                           ; pressed once
     ;; is-mouse-button-down?                                 ; Detect if a mouse button is being pressed
     ;; is-mouse-button-released?                             ; Detect if a mouse button has been released once
     ;; is-mouse-button-up?                                   ; Detect if a mouse button is NOT being pressed
     ;; get-mouse-x                                           ; Returns mouse position X
     ;; get-mouse-y                                           ; Returns mouse position Y
     get-mouse-position                                    ; Returns mouse position XY
     ;; set-mouse-position                                    ; Set mouse position XY
     ;; set-mouse-offset                                      ; Set mouse offset
     ;; set-mouse-scale                                       ; Set mouse scaling
     get-mouse-wheel-move                                  ; Returns mouse wheel movement Y

     ;; Input-related functions: touch

     ;; get-touch-x                                           ; Returns touch position X for touch point 0 (relative to screen size)
     ;; get-touch-y                                           ; Returns touch position Y for touch point 0 (relative to screen size)
     ;; get-touch-position                                    ; Returns touch position XY for a touch point index (relative to screen size)

     ;;; Gestures and Touch Handling Functions (Module: gestures)

     ;; set-gestures-enabled                                  ; Enable a set of gestures using flags
     ;; is-gesture-detected?                                  ; Check if a gesture have been detected
     ;; get-gesture-detected                                  ; Get latest detected gesture
     ;; get-touch-points-count                                ; Get touch points count
     ;; get-gesture-hold-duration                             ; Get gesture hold time in milliseconds
     ;; get-gesture-drag-vector                               ; Get gesture drag vector
     ;; get-gesture-drag-angle                                ; Get gesture drag angle
     ;; get-gesture-pinch-vector                              ; Get gesture pinch delta
     ;; get-gesture-pinch-angle                               ; Get gesture pinch angle

     ;;; Camera System Functions (Module: camera)

     set-camera-mode                                       ; Set camera mode (multiple camera
                                                           ; modes available)
     update-camera                                         ; Update camera position for
                                                           ; selected mode

     ;; set-camera-pan-control                                ; Set camera pan key to combine with mouse movement (free camera)
     ;; set-camera-alt-control                                ; Set camera alt key to combine with mouse movement (free camera)
     ;; set-camera-smooth-zoom-control                        ; Set camera smooth zoom key to combine with mouse (free camera)
     ;; set-camera-move-controls                              ; Set camera move controls (1st person and 3rd person cameras)

     ;;; Basic Shapes Drawing Functions (Module: shapes)

     ;; Basic shapes drawing functions

     ;; draw-pixel                                            ; Draw a pixel
     ;; draw-pixel-v                                          ; Draw a pixel (Vector version)
     ;; draw-line                                             ; Draw a line
     ;; draw-line-v                                           ; Draw a line (Vector version)
     ;; draw-line-ex                                          ; Draw a line defining thickness
     ;; draw-line-bezier                                      ; Draw a line using cubic-bezier curves in-out
     ;; draw-circle                                           ; Draw a color-filled circle
     ;; draw-circle-gradient                                  ; Draw a gradient-filled circle
     draw-circle-v                                         ; Draw a color-filled circle
                                                           ; (Vector version)
     ;; draw-circle-lines                                     ; Draw circle outline
     draw-rectangle                                        ; Draw a color-filled rectangle
     ;; draw-rectangle-v                                      ; Draw a color-filled rectangle (Vector version)
     draw-rectangle-rec                                    ; Draw a color-filled rectangle
     ;; draw-rectangle-pro                                    ; Draw a color-filled rectangle with pro parameters
     ;; draw-rectangle-gradient-v                             ; Draw a vertical-gradient-filled rectangle
     ;; draw-rectangle-gradient-h                             ; Draw a horizontal-gradient-filled rectangle
     ;; draw-rectangle-gradient-ex                            ; Draw a gradient-filled rectangle with custom vertex colors
     draw-rectangle-lines                                  ; Draw rectangle outline
     ;; draw-rectangle-lines-ex                               ; Draw rectangle outline with extended parameters
     ;; draw-triangle                                         ; Draw a color-filled triangle
     ;; draw-triangle-lines                                   ; Draw triangle outline
     ;; draw-poly                                             ; Draw a regular polygon (Vector version)
     ;; draw-poly-ex                                          ; Draw a closed polygon defined by points
     ;; draw-poly-ex-lines                                    ; Draw polygon lines

     ;; set-shapes-texture                                    ; Define default texture used to draw shapes

     ;;; Basic shapes collision detection functions

     ;; check-collision-recs                                  ; Check collision between two rectangles
     ;; check-collision-circles                               ; Check collision between two circles
     ;; check-collision-circle-rec                            ; Check collision between circle and rectangle
     ;; get-collision-rec                                     ; Get collision rectangle for two rectangles collision
     check-collision-point-rec                             ; Check if point is inside rectangle
     ;; check-collision-point-circle                          ; Check if point is inside circle
     ;; check-collision-point-triangle                        ; Check if point is inside a triangle

     ;;; Texture Loading and Drawing Functions (Module: textures)

     ;; Image/Texture2D data loading/unloading/saving functions

     load-image                                            ; Load image from file into CPU memory (RAM)
     ;; load-image-ex                                         ; Load image from Color array data (RGBA - 32bit)
     ;; load-image-pro                                        ; Load image from raw data with parameters
     ;; load-image-raw                                        ; Load image from RAW file data
     ;; export-image                                          ; Export image data to file
     ;; export-image-as-code                                  ; Export image as code file defining an array of bytes
     load-texture                                          ; Load texture from file into GPU memory (VRAM)
     load-texture-from-image                               ; Load texture from image data
     load-render-texture                                   ; Load texture for rendering (framebuffer)
     unload-image                                          ; Unload image from CPU memory (RAM)
     unload-texture                                        ; Unload texture from GPU memory (VRAM)
     unload-render-texture                                 ; Unload render texture from GPU memory
                                                           ; (VRAM)
     ;; get-image-data                                        ; Get pixel data from image as a Color struct array
     ;; get-image-data-normalized                             ; Get pixel data from image as Vector4 array (float normalized)
     ;; get-pixel-data-size                                   ; Get pixel data size in bytes (image or texture)
     ;; get-texture-data                                      ; Get pixel data from GPU texture and return an Image
     ;; update-texture                                        ; Update GPU texture with new data

     ;; Image manipulation functions

     ;; image-copy                                            ; Create an image duplicate (useful for transformations)
     ;; image-to-pot                                          ; Convert image to POT (power-of-two)
     ;; image-format                                          ; Convert image data to desired format
     ;; image-alpha-mask                                      ; Apply alpha mask to image
     ;; image-alpha-clear                                     ; Clear alpha channel to desired color
     ;; image-alpha-crop                                      ; Crop image depending on alpha value
     ;; image-alpha-premultiply                               ; Premultiply alpha channel
     ;; image-crop                                            ; Crop an image to a defined rectangle
     ;; image-resize                                          ; Resize image (Bicubic scaling algorithm)
     ;; image-resize-nn                                       ; Resize image (Nearest-Neighbor scaling algorithm)
     ;; image-resize-canvas                                   ; Resize canvas and fill with color
     ;; image-mipmaps                                         ; Generate all mipmap levels for a provided image
     ;; image-dither                                          ; Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
     ;; image-extract-palette                                 ; Extract color palette from image to maximum size (memory should be freed)
     ;; image-text                                            ; Create an image from text (default font)
     ;; image-text-ex                                         ; Create an image from text (custom sprite font)
     ;; image-draw                                            ; Draw a source image within a destination image
     ;; image-draw-rectangle                                  ; Draw rectangle within an image
     ;; image-draw-rectangle-lines                            ; Draw rectangle lines within an image
     ;; image-draw-text                                       ; Draw text (default font) within an image (destination)
     ;; image-draw-text-ex                                    ; Draw text (custom sprite font) within an image (destination)
     ;; image-flip-vertical                                   ; Flip image vertically
     ;; image-flip-horizontal                                 ; Flip image horizontally
     ;; image-rotate-cw                                       ; Rotate image clockwise 90deg
     ;; image-rotate-ccw                                      ; Rotate image counter-clockwise 90deg
     ;; image-color-tint                                      ; Modify image color: tint
     ;; image-color-invert                                    ; Modify image color: invert
     ;; image-color-grayscale                                 ; Modify image color: grayscale
     ;; image-color-contrast                                  ; Modify image color: contrast (-100 to 100)
     ;; image-color-brightness                                ; Modify image color: brightness (-255 to 255)
     ;; image-color-replace                                   ; Modify image color: replace color

     ;; Image generation functions

     ;; gen-image-color                                       ; Generate image: plain color
     ;; gen-image-gradient-v                                  ; Generate image: vertical gradient
     ;; gen-image-gradient-h                                  ; Generate image: horizontal gradient
     ;; gen-image-gradient-radial                             ; Generate image: radial gradient
     ;; gen-image-checked                                     ; Generate image: checked
     ;; gen-image-white-noise                                 ; Generate image: white noise
     ;; gen-image-perlin-noise                                ; Generate image: perlin noise
     ;; gen-image-cellular                                    ; Generate image: cellular algorithm. Bigger tileSize means bigger cells

     ;; Texture2D configuration functions

     gen-texture-mipmaps                                   ; Generate GPU mipmaps for a texture
     ;; set-texture-filter                                    ; Set texture scaling filter mode
     ;; set-texture-wrap                                      ; Set texture wrapping mode

     ;; Texture2D drawing functions
     draw-texture                                          ; Draw a Texture2D
     ;; draw-texture-v                                        ; Draw a Texture2D with position defined as Vector2
     ;; draw-texture-ex                                       ; Draw a Texture2D with extended parameters
     draw-texture-rec                                      ; Draw a part of a texture defined by
                                                           ; a rectangle
     ;; draw-texture-quad                                     ; Draw texture quad with tiling and offset parameters
     draw-texture-pro                                      ; Draw a part of a texture defined by
                                                           ; a rectangle with 'pro' parameters
     ;; draw-texture-npatch                                   ; Draws a texture (or part of it) that stretches or shrinks nicely

     ;;; Font Loading and Text Drawing Functions (Module: text)

     ;; Font loading/unloading functions

     ;; get-font-default                                      ; Get the default Font
     ;; load-font                                             ; Load font from file into GPU memory (VRAM)
     ;; load-font-ex                                          ; Load font from file with extended parameters
     ;; load-font-from-image                                  ; Load font from Image (XNA style)
     ;; load-font-data                                        ; Load font data for further use
     ;; gen-image-font-atlas                                  ; Generate image font atlas using chars info
     ;; unload-font                                           ; Unload Font from GPU memory (VRAM)

     ;; Text drawing functions

     draw-fps                                              ; Shows current FPS
     draw-text                                             ; Draw text (using default font)
     ;; draw-text-ex                                          ; Draw text using font and additional parameters
     ;; draw-text-rec                                         ; Draw text using font inside rectangle limits

     ;; ; Text misc. functions
     measure-text                                          ; Measure string width for default font
     ;; measure-text-ex                                       ; Measure string size for Font
     ;; get-glyph-index                                       ; Get index position for a unicode character on font

     ;; Text strings management functions
     ;; NOTE: Some strings allocate memory internally for returned strings, just be careful!

     ;; text-is-equal                                         ; Check if two text string are equal
     ;; text-length                                           ; Get text length, checks for '\0' ending
     ;; text-format                                           ; Text formatting with variables (sprintf style)
     ;; text-subtext                                          ; Get a piece of a text string
     ;; text-replace                                          ; Replace text string (memory should be freed!)
     ;; text-insert                                           ; Insert text in a position (memory should be freed!)
     ;; text-join                                             ; Join text strings with delimiter
     ;; text-split                                            ; Split text into multiple strings (memory should be freed!)
     ;; text-split-ex                                         ; Get pointers to substrings separated by delimiter
     ;; text-append                                           ; Append text at specific position and move cursor!
     ;; text-find-index                                       ; Find first text occurrence within a string
     ;; text-to-upper                                         ; Get upper case version of provided string
     ;; text-to-lower                                         ; Get lower case version of provided string
     ;; text-to-pascal                                        ; Get Pascal case notation version of provided string

     ;;; Basic 3d Shapes Drawing Functions (Module: models)

     ;; ; Basic geometric 3D shapes drawing functions
     ;; draw-line-3d                                          ; Draw a line in 3D world space
     ;; draw-circle-3d                                        ; Draw a circle in 3D world space
     draw-cube                                             ; Draw cube
     draw-cube-v                                           ; Draw cube (Vector version)
     draw-cube-wires                                       ; Draw cube wires
     ;; draw-cube-texture                                     ; Draw cube textured
     draw-sphere                                           ; Draw sphere
     ;; draw-sphere-ex                                        ; Draw sphere with extended parameters
     draw-sphere-wires                                     ; Draw sphere wires
     ;; draw-cylinder                                         ; Draw a cylinder/cone
     ;; draw-cylinder-wires                                   ; Draw a cylinder/cone wires
     draw-plane                                            ; Draw a plane XZ
     ;; draw-ray                                              ; Draw a ray line
     draw-grid                                             ; Draw a grid (centered at (0, 0, 0))
     draw-gizmo                                            ; Draw simple gizmo

     ;;; Model 3d Loading and Drawing Functions (Module: models)

     ;; Model loading/unloading functions

     load-model                                            ; Load model from files
                                                           ; (mesh and material)
     load-model-from-mesh                                  ; Load model from generated mesh
     unload-model                                          ; Unload model from memory (RAM
                                                           ; and/or VRAM)

     ;; Mesh loading/unloading functions

     ;; load-mesh                                             ; Load mesh from file
     ;; unload-mesh                                           ; Unload mesh from memory (RAM and/or VRAM)
     ;; export-mesh                                           ; Export mesh data to file

     ;; Mesh manipulation functions

     ;; mesh-bounding-box                                     ; Compute mesh bounding box limits
     ;; mesh-tangents                                         ; Compute mesh tangents
     ;; mesh-binormals                                        ; Compute mesh binormals

     ;; Mesh generation functions

     ;; gen-mesh-poly                                         ; Generate polygonal mesh
     ;; gen-mesh-plane                                        ; Generate plane mesh (with subdivisions)
     gen-mesh-cube                                            ; Generate cuboid mesh
     ;; gen-mesh-sphere                                       ; Generate sphere mesh (standard sphere)
     ;; gen-mesh-hemi-sphere                                  ; Generate half-sphere mesh (no bottom cap)
     ;; gen-mesh-cylinder                                     ; Generate cylinder mesh
     ;; gen-mesh-torus                                        ; Generate torus mesh
     ;; gen-mesh-knot                                         ; Generate trefoil knot mesh
     gen-mesh-heightmap                                    ; Generate heightmap mesh from image data
     ;; gen-mesh-cubicmap                                     ; Generate cubes-based map mesh from image data

     ;; Material loading/unloading functions

     ;; load-material                                         ; Load material from file
     ;; load-material-default                                 ; Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
     ;; unload-material                                       ; Unload material from GPU memory (VRAM)

     ;; ; Model drawing functions
     draw-model                                            ; Draw a model (with texture if set)
     ;; draw-model-ex                                         ; Draw a model with extended parameters
     ;; draw-model-wires                                      ; Draw a model wires (with texture if set)
     ;; draw-model-wires-ex                                   ; Draw a model wires (with texture if set) with extended parameters
     ;; draw-bounding-box                                     ; Draw bounding box (wires)
     draw-billboard                                        ; Draw a billboard texture
     ;; draw-billboard-rec                                    ; Draw a billboard texture defined by sourceRec

     ;; Collision detection functions

     ;; check-collision-spheres                               ; Detect collision between two spheres
     check-collision-boxes                                 ; Detect collision between two bounding boxes
     check-collision-box-sphere                            ; Detect collision between box and sphere
     ;; check-collision-ray-sphere                            ; Detect collision between ray and sphere
     ;; check-collision-ray-sphere-ex                         ; Detect collision between ray and sphere, returns collision point
     check-collision-ray-box                               ; Detect collision between ray and box
     ;; get-collision-ray-model                               ; Get collision info between ray and model
     ;; get-collision-ray-triangle                            ; Get collision info between ray and triangle
     ;; get-collision-ray-ground                              ; Get collision info between ray and ground plane (Y-normal plane)

     ;;; Shaders System Functions (Module: rlgl)
     ;;; NOTE: This functions are useless when using OpenGL 1.1

     ;; Shader loading/unloading functions

     ;; load-text                                             ; Load chars array from text file
     load-shader                                           ; Load shader from files and bind
                                                           ; default locations
     ;; load-shader-code                                      ; Load shader from code strings and bind default locations
     unload-shader                                         ; Unload shader from GPU memory (VRAM)

     ;; get-shader-default                                    ; Get default shader
     ;; get-texture-default                                   ; Get default texture

     ;; Shader configuration functions

     get-shader-location                                   ; Get shader uniform location
     set-shader-value!                                     ; Set shader uniform value
     ;; set-shader-value-v                                    ; Set shader uniform value vector
     ;; set-shader-value-matrix                               ; Set shader uniform value (matrix 4x4)
     ;; set-matrix-projection                                 ; Set a custom projection matrix (replaces internal projection matrix)
     ;; set-matrix-modelview                                  ; Set a custom modelview matrix (replaces internal modelview matrix)
     ;; get-matrix-modelview                                  ; Get internal modelview matrix

     ;; Texture maps generation (PBR)
     ;; NOTE: Required shaders should be provided

     gen-texture-cubemap                                   ; Generate cubemap texture from HDR texture
     ;; gen-texture-irradiance                                ; Generate irradiance texture using cubemap data
     ;; gen-texture-prefilter                                 ; Generate prefilter texture using cubemap data
     ;; gen-texture-brdf                                      ; Generate BRDF texture using cubemap data

     ;; Shading begin/end functions

     begin-shader-mode                                     ; Begin custom shader drawing
     end-shader-mode                                       ; End custom shader drawing (use default shader)
     begin-blend-mode                                      ; Begin blending mode (alpha, additive, multiplied)
     end-blend-mode                                        ; End blending mode (reset to default: alpha blending)
     ;; begin-scissor-mode                                    ; Begin scissor mode (define screen area for following drawing)
     ;; end-scissor-mode                                      ; End scissor mode

     ;; VR control functions

     ;; get-vr-device-info                                    ; Get VR device information for some standard devices
     ;; init-vr-simulator                                     ; Init VR simulator for selected device parameters
     ;; close-vr-simulator                                    ; Close VR simulator for current device
     ;; is-vr-simulator-ready                                 ; Detect if VR simulator is ready
     ;; set-vr-distortion-shader                              ; Set VR distortion shader for stereoscopic rendering
     ;; update-vr-tracking                                    ; Update VR tracking (position and orientation) and camera
     ;; toggle-vr-mode                                        ; Enable/Disable VR experience
     ;; begin-vr-drawing                                      ; Begin VR simulator stereo rendering
     ;; end-vr-drawing                                        ; End VR simulator stereo rendering

     ;;; Audio Loading and Playing Functions (Module: audio)

     ;; Audio device management functions
     init-audio-device                                     ; Initialize audio device and context
     close-audio-device                                    ; Close the audio device and context
     ;; is-audio-device-ready                                 ; Check if audio device has been initialized successfully
     ;; set-master-volume                                     ; Set master volume (listener)

     ;; Wave/Sound loading/unloading functions
     ;; load-wave                                             ; Load wave data from file
     ;; load-wave-ex                                          ; Load wave data from raw array data
     load-sound                                            ; Load sound from file
     ;; load-sound-from-wave                                  ; Load sound from wave data
     ;; update-sound                                          ; Update sound buffer with new data
     ;; unload-wave                                           ; Unload wave data
     unload-sound                                          ; Unload sound
     ;; export-wave                                           ; Export wave data to file
     ;; export-wave-as-code                                   ; Export wave sample data to code (.h)

     ;; Wave/Sound management functions

     play-sound                                            ; Play a sound
     ;; pause-sound                                           ; Pause a sound
     ;; resume-sound                                          ; Resume a paused sound
     ;; stop-sound                                            ; Stop playing a sound
     ;; is-sound-playing                                      ; Check if a sound is currently playing
     ;; set-sound-volume                                      ; Set volume for a sound (1.0 is max level)
     ;; set-sound-pitch                                       ; Set pitch for a sound (1.0 is base level)
     ;; wave-format                                           ; Convert wave data to desired format
     ;; wave-copy                                             ; Copy a wave to a new wave
     ;; wave-crop                                             ; Crop a wave to defined samples range
     ;; get-wave-data                                         ; Get samples data from wave as a floats array

     ;; Music management functions
     ;; load-music-stream                                     ; Load music stream from file
     ;; unload-music-stream                                   ; Unload music stream
     ;; play-music-stream                                     ; Start music playing
     ;; update-music-stream                                   ; Updates buffers for music streaming
     ;; stop-music-stream                                     ; Stop music playing
     ;; pause-music-stream                                    ; Pause music playing
     ;; resume-music-stream                                   ; Resume playing paused music
     ;; is-music-playing                                      ; Check if music is playing
     ;; set-music-volume                                      ; Set volume for music (1.0 is max level)
     ;; set-music-pitch                                       ; Set pitch for a music (1.0 is base level)
     ;; set-music-loop-count                                  ; Set music loop count (loop repeats)
     ;; get-music-time-length                                 ; Get music time length (in seconds)
     ;; get-music-time-played                                 ; Get current music time played (in seconds)

     ;; AudioStream management functions
     ;; init-audio-stream                                     ; Init audio stream (to stream raw audio pcm data)
     ;; update-audio-stream                                   ; Update audio stream buffers with data
     ;; close-audio-stream                                    ; Close audio stream and free memory
     ;; is-audio-buffer-processed?                            ; Check if any audio stream buffers requires refill
     ;; play-audio-stream                                     ; Play audio stream
     ;; pause-audio-stream                                    ; Pause audio stream
     ;; resume-audio-stream                                   ; Resume audio stream
     ;; is-audio-stream-playing?                              ; Check if audio stream is playing
     ;; stop-audio-stream                                     ; Stop audio stream
     ;; set-audio-stream-volume                               ; Set volume for audio stream (1.0 is max level)
     ;; set-audio-stream-pitch                                ; Set pitch for audio stream (1.0 is base level)

     ;; raymath

     make-matrix
     matrix-identity
     matrix-invert
     matrix-multiply
     matrix-rotate
     matrix-rotate-x
     matrix-rotate-y
     matrix-rotate-z
     vector-3-add
     vector-3-negate
     vector-3-scale
     vector-3-substract
     vector-3-transform

     ;; Data structure helper functions

     bounding-box-max
     bounding-box-min
     camera-position
     camera-target
     camera-up
     camera-fovy
     camera-type
     color-r
     color-g
     color-b
     color-a
     get-diffuse-texture
     make-bounding-box
     make-camera
     make-color
     make-ray
     make-rectangle
     make-render-texture-2d
     make-vector-2
     make-vector-3
     model-mesh
     model-transform
     model-material
     rectangle-x
     rectangle-y
     rectangle-width
     rectangle-height
     render-texture-texture
     set-cubemap-texture
     set-diffuse-texture
     set-material-shader!
     set-transform
     set-transform!
     texture-2d-width
     texture-2d-height
     vector-2-x
     vector-2-y
     vector-3-x
     vector-3-y
     vector-3-z

     ;; Utilities

     allocate-float
     allocate-int
     deg->rad
     rad->deg

     LIGHTGRAY
     GRAY
     DARKGRAY
     YELLOW
     GOLD
     ORANGE
     PINK
     RED
     MAROON
     GREEN
     LIME
     DARKGREEN
     SKYBLUE
     BLUE
     DARKBLUE
     PURPLE
     VIOLET
     DARKPURPLE
     BEIGE
     BROWN
     DARKBROWN

     WHITE
     BLACK
     BLANK
     MAGENTA
     RAYWHITE)

  (import chicken scheme foreign foreigners lolevel)
  (use foreigners lolevel)

  ;;; Macros to define foreign functions

  ; Converts signatures like
  ; ((c-pointer (struct StructType)) structName) to "*structName"
  ; Converts signatures like
  ; (int name) or ((c-pointer void) name) to "name"
  (define-for-syntax (get-argument signature)
    (let ((name (symbol->string (cadr signature))))
      (if (and (list? (car signature))
               (list? (cadar signature)))
          (string-append "*" name)
          name)))

  ; Creates foreign-lambda for C function with struct arguments passed by value.
  ; Arguments:
  ; - name of C function (string);
  ; - return type in standard Chicken FFI format;
  ; - argument list in standard Chicken FFI format.
  (define-syntax foreign-lambda-with-struct
    (er-macro-transformer
     (lambda (exp rename compare)
       (let* ((args (drop exp 3))
              (foreign-function-name (list-ref exp 1))
              (return-type (list-ref exp 2))
              (c-names (map get-argument (car args)))
              (c-names (string-join c-names ", "))
              (c-function (string-join (list foreign-function-name "(" c-names ")") ""))
              (c-call (if (eq? return-type 'void)
                          (string-join (list c-function ";") "")
                          (string-join (list "C_return(" c-function ");") ""))))
         `(foreign-lambda* ,return-type ,@args
            ,c-call)))))

  ; Creates named Scheme function for C function with struct arguments passed by value.
  ; Arguments:
  ; - Scheme function name
  ; - name of C function (string);
  ; - return type in standard Chicken FFI format;
  ; - argument list in standard Chicken FFI format.
  (define-syntax foreign-define-with-struct
    (er-macro-transformer
     (lambda (exp rename compare)
       (let* ((to-lambda (drop exp 2))
              (args (drop exp 4))
              (function-name (list-ref exp 1))
              (names (map cadr (car args))))
         `(define (,function-name ,@names)
            ((foreign-lambda-with-struct ,@to-lambda)
             ,@names))))))

  ; Creates boolean Scheme function (predicate) for C function returning 0 for false and
  ; something else for true.
  ; Arguments:
  ; - Scheme function name
  ; - name of C function (string);
  ; - return type in standard Chicken FFI format;
  ; - argument list in standard Chicken FFI format.
  (define-syntax foreign-predicate
    (er-macro-transformer
     (lambda (exp rename compare)
       (let* ((to-lambda (drop exp 2))
              (args (drop exp 4))
              (function-name (list-ref exp 1))
              (names (map cadr (car args))))
         `(define (,function-name ,@names)
            (not (= ((foreign-lambda-with-struct ,@to-lambda)
                     ,@names)
                    0)))))))

  ; Creates foreign-lambda* that creates newly allocated C structure
  ; Arguments:
  ; - Scheme function name
  ; - name of C function (string);
  ; - return type in Scheme format;
  ; - return type in standard Chicken FFI format;
  ; - list of arguments in standard Chicken FFI format.
  (define-syntax foreign-constructor
    (er-macro-transformer
     (lambda (exp rename compare)
       (let* ((rest (drop exp 5))
              (args (if (eq? rest '())
                        '()
                        (car rest)))
              (function-name (list-ref exp 1))
              (foreign-function-name (list-ref exp 2))
              (return-type (list-ref exp 3))
              (c-type (symbol->string (cadadr (list-ref exp 4))))
              (names (map cadr args))
              (c-names (map get-argument args))
              (c-names (string-join c-names ", "))
              (c-function (string-join (list foreign-function-name "(" c-names ")") ""))
              (allocation-code
               (format #f
                       "~a* new_object = (~a*)malloc(sizeof(~a));
                        *new_object = ~a;
                        C_return(new_object);"
                       c-type
                       c-type
                       c-type
                       c-function)))
         `(define (,function-name ,@names)
            (let ((new-object
                   ((foreign-lambda* ,return-type ,args
                      ,allocation-code)
                    ,@names)))
              (set-finalizer! new-object free)
              new-object))))))

  ; Creates foreign-lambda* that allocates C structure
  ; and fills its fields with given values.
  ; Argumens:
  ; - Scheme function name
  ; - return type in Scheme format;
  ; - string with C type of structure;
  ; - list of fields in standard Chicken FFI format
  (define-syntax foreign-constructor*
    (er-macro-transformer
     (lambda (exp rename compare)
       (let* ((args (car (drop exp 4)))
              (function-name (list-ref exp 1))
              (return-type (list-ref exp 2))
              (c-type (list-ref exp 3))
              (names (map cadr args))
              (c-names (map get-argument args))
              (init-field (lambda (field)
                            (format #f
                                    "new_object->~a = ~a;"
                                    (string-delete #\* field)
                                    field)))
              (init-strings (map init-field c-names))
              (allocation-code (format #f
                                       "~a* new_object = (~a*)malloc(sizeof(~a));
                                        ~a
                                        C_return(new_object);"
                                       c-type
                                       c-type
                                       c-type
                                       (string-join init-strings ""))))
         `(define (,function-name ,@names)
            (let ((new-object
                   ((foreign-lambda* ,return-type ,args
                      ,allocation-code)
                    ,@names)))
              (set-finalizer! new-object free)
              new-object))))))

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

  ;; Persistent storage management

  ;;; Input Handling Functions (Module: core)

  (foreign-predicate is-key-pressed? "IsKeyPressed" int ((int keyCode)))

  (foreign-predicate is-key-down? "IsKeyDown" int ((int keyCode)))

  (foreign-predicate is-mouse-button-pressed? "IsMouseButtonPressed" int ((int mouseButton)))

  (foreign-constructor get-mouse-position
                       "GetMousePosition"
                       vector-2
                       (c-pointer (struct Vector2)))

  (define get-mouse-wheel-move
    (foreign-lambda int "GetMouseWheelMove"))

  ;;; Camera System Functions (Module: camera)

  (foreign-define-with-struct set-camera-mode
                              "SetCameraMode"
                              void
                              (((c-pointer (struct Camera3D)) targetCamera)
                               (int cameraMode)))

  (define update-camera
    (foreign-lambda void "UpdateCamera" camera))


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

  ;; VR control functions

  ;;; Audio Loading and Playing Functions (Module: audio)

  ;; Audio device management functions

  (define init-audio-device
    (foreign-lambda void "InitAudioDevice"))

  (define close-audio-device
    (foreign-lambda void "CloseAudioDevice"))

  ;; Wave/Sound loading/unloading functions

  (foreign-constructor load-sound
                       "LoadSound"
                       sound
                       (c-pointer (struct Sound))
                       ((c-string fileName)))

  (foreign-define-with-struct unload-sound
                              "UnloadSound"
                              void
                              (((c-pointer (struct Sound)) sound)))

  ;; Wave/Sound management functions

  (foreign-define-with-struct play-sound
                              "PlaySound"
                              void
                              (((c-pointer (struct Sound)) soundToPlay)))

  ;; Music management functions

  ;; AudioStream management functions

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
    ((foreign-lambda* unsigned-short (((c-pointer (struct Color)) color))
       "C_return(color->r);")
     target-color))

  (define (color-g target-color)
    ((foreign-lambda* unsigned-short (((c-pointer (struct Color)) color))
       "C_return(color->g);")
     target-color))

  (define (color-b target-color)
    ((foreign-lambda* unsigned-short (((c-pointer (struct Color)) color))
       "C_return(color->b);")
     target-color))

  (define (color-a target-color)
    ((foreign-lambda* unsigned-short (((c-pointer (struct Color)) color))
       "C_return(color->a);")
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

  (foreign-constructor* make-color
                        color
                        "Color"
                        ((unsigned-int r)
                         (unsigned-int g)
                         (unsigned-int b)
                         (unsigned-int a)))

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

  (foreign-constructor* make-vector-2
                        vector-2
                        "Vector2"
                        ((float x)
                         (float y)))

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

  (define (vector-2-x vector)
    ((foreign-lambda* float (((c-pointer (struct Vector2)) vector))
       "C_return(vector->x);")
     vector))

  (define (vector-2-y vector)
    ((foreign-lambda* float (((c-pointer (struct Vector2)) vector))
       "C_return(vector->y);")
     vector))

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
  (define RAYWHITE   (make-color 245 245 245 255)))  ; My own White (raylib logo)
