(ql:quickload :cl-cffi-gtk)

(defpackage :bleep
  (:use :gtk :gobject :common-lisp))

(in-package :bleep)

(defvar window (gtk-window-new :toplevel))

(within-main-loop
  (g-signal-connect window "destroy" (lambda (widget)
    (declare (ignore widget))
    (leave-gtk-main)))
  (gtk-widget-show-all window))
