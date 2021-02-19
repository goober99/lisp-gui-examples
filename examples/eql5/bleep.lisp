(si::trap-fpe t nil) ; ComboBox doesn't work without this

(ql:quickload "cl-portaudio") ; Use Quicklisp to load CL-PortAudio
(qrequire :quick) ; Have EQL5 use Qt Quick module from Qt
(require :qml-lisp "qml-lisp") ; Load qml-lisp.lisp package copied from example
(use-package :qml) ; Import all external symbols from above package

; Scale used by slider
(defparameter *min-position* 0)
(defparameter *max-position* 2000)
; Range of frequencies
(defparameter *min-frequency* 20)
(defparameter *max-frequency* 20000)

; Helper function to make Lisp data available in QML
; Allows writing (set-context-property variable-in-lisp "variableInLisp" "QString")
; instead of (|setContextProperty| (|rootContext| qml:*quick-view*)
;   "variableInLisp" (qvariant-from-value variable-in-lisp "QString"))
(defun set-context-property (lisp-var qml-name type-name)
  (let ((root-context (|rootContext| qml:*quick-view*))
        (objectified (qvariant-from-value lisp-var type-name)))
    (|setContextProperty| root-context qml-name objectified)))

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
            (make-array frames-per-buffer :initial-contents
              (loop for j from (+ (* frames-per-buffer i) 1) to (* frames-per-buffer (+ i 1)) collect
                (let ((time (/ j sample-rate)))
                  (* amplitude (sin (* 2 pi frequency time))))))))))))

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

; Buttons increase and decrease frequency by one octave
(defun adjust-octave (modifier)
  (let* ((freq (q< |frequency| "root"))
         (new-freq (* freq modifier)))
    (unless (or (< new-freq *min-frequency*) (> new-freq *max-frequency*))
      (q> |frequency| "root" new-freq))))
(defun decrease-octave () (adjust-octave 0.5))
(defun increase-octave () (adjust-octave 2))

; Use the desktop-oriented style Fusion instead of the default
; https://doc.qt.io/QT-5/qtquickcontrols2-styles.html
; EQL5 doesn't wrap the QQuickStyle class so using environment variable
(ext:setenv "QT_QUICK_CONTROLS_STYLE" "fusion")

(defun run ()
  ; QQuickView provides a window for displaying a Qt Quick user interface:
  ; https://doc.qt.io/qt-5/qquickview.html
  (setf qml:*quick-view* (qnew "QQuickView"))

  ; Make data available in QML
  (set-context-property *min-position* "minPosition" "int")
  (set-context-property *max-position* "maxPosition" "int")
  (set-context-property *min-frequency* "minFrequency" "int")
  (set-context-property *max-frequency* "maxFrequency" "int")

  (x:do-with qml:*quick-view*
    (|setSource| (|fromLocalFile.QUrl| "bleep.qml"))
    (|setTitle| "Bleep")
    (|show|)))

(run)
