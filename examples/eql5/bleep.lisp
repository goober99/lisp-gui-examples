(si::trap-fpe t nil) ; ignore floating point overflows

(qrequire :quick) ; Have EQL5 use Qt Quick module from Qt
(require :qml-lisp "qml-lisp") ; Load qml-list.lisp package copied from example
(use-package :qml) ; Import all external symbols from above package

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

; Use the desktop-oriented style Fusion instead of the default
; https://doc.qt.io/QT-5/qtquickcontrols2-styles.html
; EQL5 doesn't wrap the QQuickStyle class so using environment variable
; use ext:setenv to set an environment variable
; https://common-lisp.net/project/ecl/static/manual/Operating-System-Interface.html
(ext:setenv "QT_QUICK_CONTROLS_STYLE" "fusion")

(defun run ()
  ; The *quick-view* variable is defined in qml-lisp.lisp
  ; (qnew "Class") creates a new Qt object of class
  ; QQuickView provides a window for displaying a Qt Quick user interface:
  ; https://doc.qt.io/qt-5/qquickview.html
  (setf qml:*quick-view* (qnew "QQuickView"))
  ; EQL5 x package defined in src/lisp/x.lisp (appears to be utility functions)
  ; No documentation for do-with other than macro definition
  ; I don't want to take the time to parse the definition
  (x:do-with qml:*quick-view*
    ; Wrapper functions |function| are the most convenient way of calling Qt methods
    ; It is an alternative to (qfun object function-name arguments)
    ; e.g. (qfun qml:*quick-view* "show")
    (|setSource| (|fromLocalFile.QUrl| "bleep.qml"))
    (|setTitle| "Bleep")
    (|show|)))

(run)
