(si::trap-fpe t nil) ; ignore floating point overflows

(qrequire :quick) ; Have EQL5 use Qt Quick module from Qt
(require :qml-lisp "qml-lisp") ; Load qml-list.lisp package copied from example
(use-package :qml) ; Import all external symbols from above package

(write-line (ext:getenv "SHELL"))
; use ext: setenv to set an environment variable
; https://common-lisp.net/project/ecl/static/manual/Operating-System-Interface.html

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
