; Load GTklos
(require "gtklos")
(import GTKLOS)

; Main window
(define window (make <vwindow> #:title "Bleep" #:border-width 25))

(define slider (make <scale> #:parent window
                             #:orientation 'horizontal
                             #:draw-value #f
                             #:from 20
                             #:to 20000
                             #:value 440))

(gtk-main)
