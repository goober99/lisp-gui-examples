(ql:quickload :cl-cffi-gtk)

; Scale used by slider
(defparameter *min-position* 0)
(defparameter *max-position* 2000)
; Range of frequencies
(defparameter *min-frequency* 20)
(defparameter *max-frequency* 20000)

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
; Return value: The gtk-spin-button instance inside the container
(defun units-spin-button (from to initial units)
  (let ((container (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 10))
        (spin-button (make-instance 'gtk:gtk-spin-button
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

; Link slider to text field display of frequency
(defun adjust-frequency (adjustment)
  (setf (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field))
        (position->frequency (gtk:gtk-adjustment-value adjustment))))
(defun adjust-slider (adjustment)
  (setf (gtk:gtk-adjustment-value (gtk:gtk-range-adjustment slider))
        (frequency->position (gtk:gtk-adjustment-value adjustment))))

; Buttons increase and decrease frequency by one octave
(defun set-frequency (freq)
  (setf (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field)) freq))
(defun adjust-octave (modifier)
  (set-frequency (* (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field)) modifier)))
(defun decrease-octave (widget) (adjust-octave 0.5))
(defun increase-octave (widget) (adjust-octave 2))

(defvar slider (make-instance 'gtk:gtk-scale
                              :orientation :horizontal
                              :draw-value nil
                              :adjustment
                              (make-instance 'gtk:gtk-adjustment
                                             :value (frequency->position 440)
                                             :lower *min-position*
                                             :upper *max-position*
                                             :step-increment 1)))
(gobject:g-signal-connect (gtk:gtk-range-adjustment slider) "value-changed" #'adjust-frequency)
(gtk:gtk-box-pack-start vbox slider)

(defvar frequency-box (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 25))
(defvar lower-button (make-instance 'gtk:gtk-button :label "<"))
(gobject:g-signal-connect lower-button "clicked" #'decrease-octave)
(gtk:gtk-box-pack-start frequency-box lower-button :fill nil)
(defvar frequency-field (units-spin-button *min-frequency* *max-frequency* 440 "Hz"))
(gobject:g-signal-connect (gtk:gtk-spin-button-adjustment frequency-field) "value-changed" #'adjust-slider)
(gtk:gtk-box-pack-start frequency-box (gtk:gtk-widget-parent frequency-field) :fill nil)
(defvar higher-button (make-instance 'gtk:gtk-button :label ">"))
(gobject:g-signal-connect higher-button "clicked" #'increase-octave)
(gtk:gtk-box-pack-start frequency-box higher-button :fill nil)
(gtk:gtk-box-pack-start vbox frequency-box)

(gtk:within-main-loop
  ; Quit program when window closed
  (gobject:g-signal-connect window "destroy" (lambda (widget)
    (declare (ignore widget))
    (gtk:leave-gtk-main)))
  ; Display GUI
  (gtk:gtk-container-add window vbox)
  (gtk:gtk-widget-show-all window))
