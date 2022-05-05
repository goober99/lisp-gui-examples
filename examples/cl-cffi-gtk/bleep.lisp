(ql:quickload :cl-cffi-gtk)
(ql:quickload :cl-portaudio)

; Scale used by slider
(defparameter *min-position* 0)
(defparameter *max-position* 2000)
; Range of frequencies
(defparameter *min-frequency* 20)
(defparameter *max-frequency* 20000)

; Notes -> frequency (middle A-G [A4-G4])
; http://pages.mtu.edu/~suits/notefreqs.html
(defvar notes '(("A" . 440.00)
                ("B" . 493.88)
                ("C" . 261.63)
                ("D" . 293.66)
                ("E" . 329.63)
                ("F" . 349.23)
                ("G" . 292.00)))

; Generate a tone using CL-PortAudio
(defun generate-tone (frequency duration)
  (let ((frames-per-buffer 1024)
        (sample-rate 44100d0)
        (amplitude 0.5))
    ; Initialize PortAudio environment
    (portaudio:with-audio
      ; Open and start audio stream
      (portaudio:with-default-audio-stream (astream 1 1
                                            :sample-format :float
                                            :sample-rate sample-rate
                                            :frames-per-buffer frames-per-buffer)
        (dotimes (i (round (/ (* (/ duration 1000) sample-rate) frames-per-buffer)))
          ; Write buffer to output stream
          (portaudio:write-stream astream
            ; portaudio:write-stream requires an array as input, not a list
            (make-array frames-per-buffer :element-type 'single-float :initial-contents
              (loop for j from (+ (* frames-per-buffer i) 1) to (* frames-per-buffer (+ i 1)) collect
                (let ((time (/ j sample-rate)))
                  ; Since sample-rate and pi are double-float, they make result
                  ; double-float. PortAudio expects single-float, and will warn
                  ; when run with SBCL if not given single-float.
                  (coerce (* amplitude (sin (* 2 pi frequency time))) 'single-float))))))))))

; Logarithmic scale for frequency (so middle A [440] falls about in the middle)
; Adapted from https://stackoverflow.com/questions/846221/logarithmic-slider

(defvar min-freq (log *min-frequency*))
(defvar max-freq (log *max-frequency*))
(defvar frequency-scale (/ (- max-freq min-freq) (- *max-position* *min-position*)))
; Convert slider position to frequency
(defun position->frequency (position)
  (round (exp (+ min-freq (* frequency-scale (- position *min-position*))))))
; Convert frequency to slider position
(defun frequency->position (freq)
  (round (/ (- (log freq) min-freq) (+ frequency-scale *min-position*))))

; Create a spin button with a units label
; Arguments: from - number, lower bound of range),
;            to - number, upper bound of range
;            initial - number, initial value of spin button
;            units - string, label after spin button
; Optional Arguments: digits - number <= 20, number of decimal places to show
; Return value: The gtk-spin-button instance inside the container
(defun units-spin-button (from to initial units &key (digits 0))
  (let ((container (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 10))
        (spin-button (make-instance 'gtk:gtk-spin-button
                                    :digits digits
                                    :adjustment
                                    (make-instance 'gtk:gtk-adjustment
                                                   :value initial
                                                   :lower from
                                                   :upper to
                                                   :step-increment 1)))
        (label (make-instance 'gtk:gtk-label :label units)))
    (gtk:gtk-box-pack-start container spin-button :fill nil)
    (gtk:gtk-box-pack-start container label :fill nil)
    ; Return the container holding the spin button and label
    spin-button))

; Main window
(defvar window (make-instance 'gtk:gtk-window :type :toplevel :title "Bleep"))
(defvar vbox (make-instance 'gtk:gtk-box :orientation :vertical
                                         :spacing 25
                                         :margin 25))

; Frequency controls

(defvar slider (make-instance 'gtk:gtk-scale
                              :orientation :horizontal
                              :draw-value nil
                              :adjustment
                              (make-instance 'gtk:gtk-adjustment
                                             :value (frequency->position 440)
                                             :lower *min-position*
                                             :upper *max-position*
                                             :step-increment 1)))
(gtk:gtk-box-pack-start vbox slider)
(defvar frequency-field (units-spin-button *min-frequency* *max-frequency* 440 "Hz" :digits 2))

; Link slider to text field display of frequency
(gobject:g-signal-connect slider "change-value"
  ; Connect to change-value signal of slider instead of value-changed signal of
  ; its corresponding adjustment object so that frequency will only be updated
  ; when interactively moved avoiding rounding differences between slider and
  ; sping button.
  (lambda (range scroll value)
    (declare (ignore range scroll))
    (setf (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field))
          (position->frequency value))))
(gobject:g-signal-connect (gtk:gtk-spin-button-adjustment frequency-field) "value-changed"
  (lambda (adjustment)
    (setf (gtk:gtk-adjustment-value (gtk:gtk-range-adjustment slider))
          (frequency->position (gtk:gtk-adjustment-value adjustment)))))

; Buttons increase and decrease frequency by one octave
(defun set-frequency (freq)
  (setf (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field)) freq))
(defun adjust-octave (modifier)
  (set-frequency (* (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field)) modifier)))
(defun decrease-octave (widget) (declare (ignore widget)) (adjust-octave 0.5))
(defun increase-octave (widget) (declare (ignore widget)) (adjust-octave 2))

(defvar lower-button (make-instance 'gtk:gtk-button :label "<"))
(gobject:g-signal-connect lower-button "clicked" #'decrease-octave)
(defvar higher-button (make-instance 'gtk:gtk-button :label ">"))
(gobject:g-signal-connect higher-button "clicked" #'increase-octave)

(defvar frequency-box (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 25))
(gtk:gtk-box-pack-start frequency-box lower-button :fill nil)
(gtk:gtk-box-pack-start frequency-box (gtk:gtk-widget-parent frequency-field) :fill nil)
(gtk:gtk-box-pack-start frequency-box higher-button :fill nil)
(gtk:gtk-box-pack-start vbox frequency-box)

; General Controls

(defvar control-box (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 25))
(defvar duration-field (units-spin-button 1 600000 200 "ms"))
(gtk:gtk-box-pack-start control-box (gtk:gtk-widget-parent duration-field) :fill nil)
(defvar play-button (make-instance 'gtk:gtk-button :label "Play"))
(gobject:g-signal-connect play-button "clicked" (lambda (widget)
  (declare (ignore widget))
  (generate-tone (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field))
                 (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment duration-field)))))
(gtk:gtk-box-pack-start control-box play-button :fill nil)

; Create combo box and label
(defvar note-box (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 10))
(defvar note-label (make-instance 'gtk:gtk-label :label "♪"))
(gtk:gtk-box-pack-start note-box note-label :fill nil)
(defvar note (make-instance 'gtk:gtk-combo-box-text))
(gtk:gtk-box-pack-start note-box note :fill nil)
; Populate combo box
(gtk:gtk-combo-box-text-append-text note "A")
(gtk:gtk-combo-box-text-append-text note "B")
(gtk:gtk-combo-box-text-append-text note "C")
(gtk:gtk-combo-box-text-append-text note "D")
(gtk:gtk-combo-box-text-append-text note "E")
(gtk:gtk-combo-box-text-append-text note "F")
(gtk:gtk-combo-box-text-append-text note "G")
; Set frequency to specific note
(gobject:g-signal-connect note "changed"
  (lambda (object)
    ; Changed to gtk-combo-box-text-active-text if Quicklisp gets updated with
    ; the version of cl-cffi-gtk in the crategus Git.
    (let ((value (gtk:gtk-combo-box-text-get-active-text object)))
      (set-frequency (cdr (assoc value notes :test 'equal))))))
; Pack the combo box
(gtk:gtk-box-pack-start control-box note-box :fill nil)
(gtk:gtk-box-pack-start vbox control-box)

(gtk:within-main-loop
  ; Quit program when window closed
  (gobject:g-signal-connect window "destroy" (lambda (widget)
    (declare (ignore widget))
    (gtk:leave-gtk-main)))
  ; Display GUI
  (gtk:gtk-container-add window vbox)
  (gtk:gtk-widget-show-all window))
