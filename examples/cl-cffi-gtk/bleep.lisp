(ql:quickload :cl-cffi-gtk)

(defpackage :bleep
  (:use :gtk :gobject :common-lisp))

(in-package :bleep)

(within-main-loop
  (let ((window (gtk-window-new :toplevel)))
    (g-signal-connect window "destroy" (lambda (widget)
      (declare (ignore widget))
      (leave-gtk-main)))
    (gtk-widget-show-all window)))
