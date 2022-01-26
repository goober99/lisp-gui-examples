(import iup)

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

; Slider and frequency controls
(define slider
  (valuator
    min: *min-position*
    max: *max-position*
    value: (frequency->position 440)
    expand: 'Yes))
(define frequency-controls
  (hbox
    alignment: 'ACENTER
    gap: 25
    margin: '0x0
    (button title: '<)
    (hbox
      alignment: 'ACENTER
      gap: 5
      (textbox
        spin: 'Yes
        spinmin: *min-frequency*
        spinmax: *max-frequency*
        spinvalue: 440)
      (label "Hz"))
    (button title: '>)))

; Main window
(define dlg
  (dialog
    title: "Bleep"
    (vbox
      alignment: 'ACENTER
      gap: 25
      margin: '25x25
      slider
      frequency-controls)))

(show dlg)
(main-loop)
(destroy! dlg)
(exit 0)
