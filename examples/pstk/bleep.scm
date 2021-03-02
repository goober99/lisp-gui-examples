(import pstk)

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

(tk-start)
(ttk-map-widgets 'all) ; Use the Ttk widget set

; Create a spin box with a units label
; Returns frame widget encompassing both spin box and label and the spin box
; widget itself. This way you can access the value of the spin box.
; e.g. (define-values (box-with-label just-box) (units-spinbox 1 12 6 "inches"))
(define (units-spinbox from to initial units)
  (let* ((container (tk 'create-widget 'frame))
         (spinbox (container 'create-widget 'spinbox 'from: from 'to: to))
         (label (container 'create-widget 'label 'text: units)))
    (spinbox 'set initial)
    (tk/pack spinbox label 'side: 'left 'padx: 2)
    (values container spinbox)))

; Main window
(tk/wm 'title tk "Bleep")

; Frequency controls

; Set frequency slider and display
(define (set-frequency freq)
  ;(tk-with-lock (lambda ()
    (print "button setting spin box to " freq)
    (frequency-int 'set freq)
    (print "button setting slider to " freq "->" (frequency->position freq))
    ;(slider 'set (frequency->position freq))
    (print "octave button complete"));))

; Buttons increase and decrease frequency by one octave
(define (adjust-octave modifier)
  (set-frequency (* (string->number (frequency-int 'get)) modifier)))
(define (decrease-octave) (adjust-octave 0.5))
(define (increase-octave) (adjust-octave 2))

(define slider (tk 'create-widget 'scale 'from: *min-position* 'to: *max-position*))
(slider 'set (frequency->position 440))
(slider 'configure 'command: (lambda (x)
  (print "slider triggered on " x)
  (frequency-int 'set (position->frequency x))
  (print "slider callback complete")))
(define lower-button (tk 'create-widget 'button 'text: "<" 'command: decrease-octave))
(define-values (frequency-ext frequency-int)
  (units-spinbox *min-frequency* *max-frequency* 440 "Hz"))
(define higher-button (tk 'create-widget 'button 'text: ">" 'command: increase-octave))

; Layout widgets in a grid
(tk/grid slider 'row: 0 'columnspan: 3 'sticky: 'ew 'padx: 20 'pady: 20)
(tk/grid lower-button 'row: 1 'column: 0 'padx: 20 'pady: 20)
(tk/grid frequency-ext 'row: 1 'column: 1 'padx: 20 'pady: 20)
(tk/grid higher-button 'row: 1 'column: 2 'padx: 20 'pady: 20)

(tk-event-loop)
