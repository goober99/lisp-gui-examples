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

; PORT TO COMMON LISP/GTK AND USE FOR FREQUENCY SPINBOX
; ADJUSTMENT OBJECT CAN BE DEFINED OUTSIDE OF THIS FUNCTION
; THIS FUNCTION IS FOR DISPLAY PURPOSES
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
(defvar window (make-instance 'gtk:gtk-window :type :toplevel
                                              :title "Bleep"))
(defvar vbox (make-instance 'gtk:gtk-box :orientation :vertical
                                                      :spacing 25
                                                      :margin 25))

; Frequency controls

(defvar slider-position (make-instance 'gtk:gtk-adjustment :value (frequency->position 440)
                                                           :lower *min-position*
                                                           :upper *max-position*
                                                           :step-increment 1))
(defvar slider (make-instance 'gtk:gtk-scale :orientation :horizontal
                                             :draw-value nil
                                             :adjustment slider-position))
(gtk:gtk-box-pack-start vbox slider)
(defvar frequency-box (make-instance 'gtk:gtk-box :orientation :horizontal
                                                  :spacing 25))
(defvar lower-button (make-instance 'gtk:gtk-button :label "<"))
(gtk:gtk-box-pack-start frequency-box lower-button :fill nil)
(defvar frequency (make-instance 'gtk:gtk-adjustment :value 440
                                                     :lower *min-frequency*
                                                     :upper *max-frequency*
                                                     :step-increment 1))
(defvar frequency-field (make-instance 'gtk:gtk-spin-button :adjustment frequency))
(gtk:gtk-box-pack-start frequency-box frequency-field :fill nil)
(defvar frequency-label (make-instance 'gtk:gtk-label :label "Hz"))
(gtk:gtk-box-pack-start frequency-box frequency-label :fill nil)
(defvar higher-button (make-instance 'gtk:gtk-button :label ">"))
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
