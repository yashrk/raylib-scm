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

