(import iup)
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

; Slider and frequency controls
(define slider
  (valuator
    min: *min-position*
    max: *max-position*
    value: (frequency->position 440)
    expand: 'Yes
    valuechanged-cb: (lambda (self)
      (attribute-set! frequency-field spinvalue:
        (position->frequency (string->number (attribute self value:))))
      'default)))
(define frequency-field
  (textbox
    spin: 'Yes
    spinmin: *min-frequency*
    spinmax: *max-frequency*
    spinvalue: 440
    valuechanged-cb: (lambda (self)
      (attribute-set! slider value:
        (frequency->position (string->number (attribute self spinvalue:)))))))
; Set frequency slider and display
(define (set-frequency freq)
  (when (and (>= freq *min-frequency*) (<= freq *max-frequency*))
    (attribute-set! slider value: (frequency->position freq))
    (attribute-set! frequency-field value: freq)))
; Buttons increase and decrease frequency by one octave
(define (adjust-octave modifier)
  (set-frequency (* (string->number (attribute frequency-field spinvalue:)) modifier)))
(define (decrease-octave self) (adjust-octave 0.5))
(define (increase-octave self) (adjust-octave 2))
(define frequency-controls
  (hbox
    alignment: 'ACENTER
    gap: 25
    margin: '0x0
    (button
      title: '<
      action: decrease-octave)
    (hbox
      alignment: 'ACENTER
      gap: 5
      frequency-field
      (label "Hz"))
    (button
      title: '>
      action: increase-octave)))

; General controls
(define duration-field
  (textbox
    spin: 'Yes
    spinmin: 1
    spinmax: 600000 ; 10 minutes
    spinvalue: 200))
(define general-controls
  (hbox
    alignment: 'ACENTER
    gap: 25
    margin: '0x0
    (hbox
      alignment: 'ACENTER
      gap: 5
      duration-field
      (label "ms"))
    (button
      title: "Play"
      action: (lambda (self)
        (generate-tone (string->number (attribute frequency-field spinvalue:))
                       (string->number (attribute duration-field spinvalue:)))))
    (hbox
      alignment: 'ACENTER
      gap: 5
      (label "♪")
      (listbox
        #:1 "A" #:2 "B" #:3 "C" #:4 "D" #:5 "E" #:6 "F" #:7 "G"
        value: 1
        dropdown: 'Yes
        action: (lambda (self text item state)
          (set-frequency (cadr (assoc text notes))))))))

; Main window
(define dlg
  (dialog
    title: "Bleep"
    (vbox
      alignment: 'ACENTER
      gap: 25
      margin: '25x25
      slider
      frequency-controls
      general-controls)))

; Display GUI
(show dlg)

; Initialize Allegro and audio addon
; Must be initialized afer showing the main dialog but before starting the main
; loop or else program will die with a segmentation violation.
(unless (al:init) (print "Could not initialize Allegro."))
(unless (al:audio-addon-install) (print "Could not initialize sound."))
(al:reserve-samples 0)

; Start IUP main loop
(main-loop)
(destroy! dlg)
(exit 0)
