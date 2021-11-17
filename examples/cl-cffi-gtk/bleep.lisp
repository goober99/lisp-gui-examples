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

; Main window
(defvar window (make-instance 'gtk:gtk-window :type :toplevel
                                              :title "Bleep"))
(defvar vbox (make-instance 'gtk:gtk-box :orientation :vertical
                                                      :spacing 10))

; Frequency controls

(defvar frequency (make-instance 'gtk:gtk-adjustment :value (frequency->position 440)
                                                     :lower *min-position*
                                                     :upper *max-position*
                                                     :step-increment 1))
(defvar slider (make-instance 'gtk:gtk-scale :orientation :horizontal
                                             :draw-value nil
                                             :width-request 200
                                             :adjustment frequency
                                             :margin 25))
(defvar frequency-box (make-instance 'gtk:gtk-box :orientation :horizontal
                                                  :spacing 10))
(defvar lower-button (make-instance 'gtk:gtk-button :label "<"))
;(define frequency-field (new text-field% [label #f]
;                                         [parent frequency-pane]
;                                         [init-value "440"]
;                                         [min-width 64]
;                                         [stretchable-width #f]))
;(define frequency-label (new message% [parent frequency-pane] [label "Hz"]))
(defvar higher-button (make-instance 'gtk:gtk-button :label ">"))

(gtk:gtk-box-pack-start vbox slider)
(gtk:gtk-box-pack-start frequency-box lower-button)
(gtk:gtk-box-pack-start frequency-box higher-button)
(gtk:gtk-box-pack-start vbox frequency-box)
(gtk:gtk-container-add window vbox)

(gtk:within-main-loop
  ; Quit program when window closed
  (gobject:g-signal-connect window "destroy" (lambda (widget)
    (declare (ignore widget))
    (gtk:leave-gtk-main)))
  ; Display GUI
  (gtk:gtk-widget-show-all window))
