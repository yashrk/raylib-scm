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
