; Load GTklos
(require "gtklos")
(import GTKLOS)

; Scale used by slider
(define *min-position* 0)
(define *max-position* 2000)
; Range of frequencies
(define *min-frequency* 20)
(define *max-frequency* 20000)

; Logarithmic scale for frequency (so middle A [440] falls about in the middle)
; Adapted from https://stackoverflow.com/questions/846221/logarithmic-slider

(define min-freq (log *min-frequency*))
(define max-freq (log *max-frequency*))
(define frequency-scale (/ (- max-freq min-freq) (- *max-position* *min-position*)))
; Convert slider position to frequency
(define (position->frequency position)
  (round (exp (+ min-freq (* frequency-scale (- position *min-position*))))))
; Convert frequency to slider position
(define (frequency->position freq)
  (round (/ (- (log freq) min-freq) (+ frequency-scale *min-position*))))

; Main window
(define window (make <vwindow> #:title "Bleep" #:border-width 25 #:spacing 25))

(define slider (make <scale> #:parent window
                             #:orientation 'horizontal
                             #:draw-value #f
                             #:from *min-position*
                             #:to *max-position*
                             #:value (frequency->position 440)))

(define frequency-pane (make <hbox> #:parent window #:spacing 25))
(define lower-button (make <button> #:parent frequency-pane #:text "<"))
(define frequency-control (make <hbox> #:parent frequency-pane #:spacing 10))
(define frequency-field (make <entry> #:parent frequency-control #:value "440"))
(define frequency-label (make <label> #:parent frequency-control #:text "Hz"))
(define higher-button (make <button> #:parent frequency-pane #:text ">"))

(gtk-main)
