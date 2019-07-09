;;; Font Loading and Text Drawing Functions (Module: text)

(define draw-fps
  (foreign-lambda void "DrawFPS" int int))

(define (draw-text text pos-x pos-y font-size text-color)
  ((foreign-lambda* void ((c-string text)
                          (int posX)
                          (int posY)
                          (int fontSize)
                          (rayc textColor))
     "DrawText(text, posX, posY, fontSize, *((Color*)textColor));")
   text pos-x pos-y font-size text-color))

(foreign-define-with-struct measure-text
                            "MeasureText"
                            int
                            ((c-string text) (int fontSize)))
