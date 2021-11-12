(import pstk)
(import (prefix allegro "al:"))
(import (chicken memory))

(define +pi+ 3.141592)

; Scale used by slider
(define *min-position* 0)
(define *max-position* 2000)
; Range of frequencies
(define *min-frequency* 20)
(define *max-frequency* 20000)

; Notes -> frequency (middle A-G [A4-G4])
; http://pages.mtu.edu/~suits/notefreqs.html
(define notes '(("A" 440.00)
                ("B" 493.88)
                ("C" 261.63)
                ("D" 293.66)
                ("E" 329.63)
                ("F" 349.23)
                ("G" 292.00)))

; Initialize Allegro and audio addon
(unless (al:init) (print "Could not initialize Allegro."))
(unless (al:audio-addon-install) (print "Could not initialize sound."))
(al:reserve-samples 0)

; Generate a tone using Allegro
(define (generate-tone frequency duration)
  (let* ((samples-per-buffer 1024)
         (stream-frequency 44100)
         (amplitude 0.5)
         (stream (al:make-audio-stream 8 samples-per-buffer stream-frequency 'float32 'one))
         (queue (al:make-event-queue))
         (event (al:make-event)))

    (unless (al:audio-stream-attach-to-mixer! stream (al:default-mixer))
      (print "Could not attach stream to mixer."))
    (al:event-queue-register-source! queue (al:audio-stream-event-source stream))

    (let event-loop ((n 0))
      ; Grab and handle events
      (when (and (< n (/ (* (/ duration 1000) stream-frequency) samples-per-buffer))
                 (al:event-queue-wait! queue event))
        (case (al:event-type event) ('audio-stream-fragment
          (let ((buffer (al:audio-stream-fragment stream)))
            ; If the stream is not ready for new data, buffer will be null.
            (if (not buffer) (event-loop n) (begin
              (let ((adr (pointer->address buffer)))
                (let loop ((i 0))
                  (when (< i samples-per-buffer)
                    (let ((time (/ (+ (* samples-per-buffer n) i) stream-frequency)))
                      ; al:audio-stream-fragment returns a C pointer. Use (chicken
                      ; memory) module to operate on foreign pointer objects.
                      ; Iterate over array four bytes at a time since 32-bit depth.
                      (pointer-f32-set! (address->pointer (+ adr (* i 4)))
                        (* amplitude (sin (* 2 +pi+ frequency time))))
                      (loop (+ i 1)))))
                (unless (al:audio-stream-fragment-set! stream buffer)
                  (print "Error setting stream fragment")))
              ; Repeat
              (event-loop (+ n 1)))))))))

    (al:audio-stream-drain stream)))

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
         (spinbox (container 'create-widget 'spinbox 'from: from 'to: to
                             'width: (+ 4 (string-length (number->string to)))))
         (label (container 'create-widget 'label 'text: units)))
    (spinbox 'set initial)
    (tk/bind spinbox '<FocusOut> (lambda ()
      (let ((current-value (string->number (spinbox 'get))))
        (unless (and current-value
                     (>= current-value from)
                     (<= current-value to))
          (spinbox 'set from)
          ; Also reset slider position to make sure it still matches display
          (slider 'configure 'value: (frequency->position (string->number (frequency-int 'get))))))))
    (tk/pack spinbox label 'side: 'left 'padx: 2)
    (values container spinbox)))

; Main window
(tk/wm 'title tk "Bleep")

; Frequency controls

; Set frequency slider and display
(define (set-frequency freq)
  (when (and (>= freq *min-frequency*) (<= freq *max-frequency*))
    (slider 'configure 'value: (frequency->position freq))
    (frequency-int 'set freq)))

; Buttons increase and decrease frequency by one octave
(define (adjust-octave modifier)
  (set-frequency (* (string->number (frequency-int 'get)) modifier)))
(define (decrease-octave) (adjust-octave 0.5))
(define (increase-octave) (adjust-octave 2))

(define slider (tk 'create-widget 'scale 'from: *min-position* 'to: *max-position*
                   'command: (lambda (x) (frequency-int 'set (position->frequency x)))))
(slider 'configure 'value: (frequency->position 440))
(define lower-button (tk 'create-widget 'button 'text: "<" 'command: decrease-octave))
(define-values (frequency-ext frequency-int)
  (units-spinbox *min-frequency* *max-frequency* 440 "Hz"))
(tk/bind frequency-int '<KeyRelease> (lambda ()
  ; If frequency value is a valid number, set slider to current value
  (let ((numified (string->number (frequency-int 'get))))
    (when numified
      (slider 'configure 'value: (frequency->position numified))))))
(define higher-button (tk 'create-widget 'button 'text: ">" 'command: increase-octave))

; General controls

(define-values (duration-ext duration-int) (units-spinbox 1 600000 200 "ms"))
(define play-button (tk 'create-widget 'button 'text: "Play" 'command: (lambda ()
  (generate-tone (string->number (frequency-int 'get)) (string->number (duration-int 'get))))))
(define note-frame (tk 'create-widget 'frame))
(define note (note-frame 'create-widget 'combobox 'width: 2 'values: '("A" "B" "C" "D" "E" "F" "G")))
(tk/bind note '<<ComboboxSelected>> (lambda () (set-frequency (cadr (assoc (note 'get) notes)))))
(define note-label (note-frame 'create-widget 'label 'text: "♪"))
(tk/pack note-label note 'side: 'left 'padx: 2)

; Layout widgets in a grid
(tk/grid slider 'row: 0 'columnspan: 3 'sticky: 'ew 'padx: 20 'pady: 20)
(tk/grid lower-button 'row: 1 'column: 0 'padx: 20 'pady: 20)
(tk/grid frequency-ext 'row: 1 'column: 1 'padx: 20 'pady: 20)
(tk/grid higher-button 'row: 1 'column: 2 'padx: 20 'pady: 20)
(tk/grid duration-ext 'row: 2 'column: 0 'padx: 20 'pady: 20)
(tk/grid play-button 'row: 2 'column: 1 'padx: 20 'pady: 20)
(tk/grid note-frame 'row: 2 'column: 2 'padx: 20 'pady: 20)

(tk-event-loop)
