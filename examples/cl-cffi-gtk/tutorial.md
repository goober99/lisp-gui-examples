The cl-cffi-gtk library provides Common Lisp bindings to GTK. The library is
developed with SBCL but should also work with Clozure CL and CLISP. For this
tutorial, I'll be using SBCL. Instead of building yet another calculator, let's
build a GUI for generating a tone.

![Screenshot](../../screenshots/racket.png?raw=true "Example screenshot")

You'll need SBCL installed. It's available in the repositories of most Linux
distros, so just install it from your distro's repo. Depending on your destkop
environment, you probably already have GTK installed (even if you use KDE, it's
highly likely you already have GTK installed.) If you're on Windows or macOS,
you'll probably have to install GTK in addition to SBCL.

The Common Lisp bindings to GTK can be installed with
[Quicklisp](https://www.quicklisp.org/). If you don't already have Quicklisp
installed, it's painless. See the Quicklisp website for more details, but
here's an example of installing Quicklisp on Debian and configuring SBCL. The
steps should be the same for any Linux distro and macOS.

```console
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load quicklisp.lisp
> (quicklisp-quickstart:install)
> (ql:add-to-init-file)
```

The "proper" way to include a dependency would be to use
[ASDF](https://common-lisp.net/project/asdf/) and create a `.asd` file for the
project. Since this is a quick tutorial, I'll use with `ql:quickload`.

```lisp
(ql:quickload :cl-cffi-gtk)

```

The first time you run `bleep.lisp`, it will take awhile as Quicklisp downloads
cl-cffi-gtk (by default it will be downloaded to `~/quicklisp`).

```lisp
; Main window
(defvar window (make-instance 'gtk:gtk-window :type :toplevel :title "Bleep"))
(defvar vbox (make-instance 'gtk:gtk-box :orientation :vertical
                                                      :spacing 25
                                                      :margin 25))

(gtk:within-main-loop
  ; Quit program when window closed
  (gobject:g-signal-connect window "destroy" (lambda (widget)
    (declare (ignore widget))
    (gtk:leave-gtk-main)))
  ; Display GUI
  (gtk:gtk-container-add window vbox)
  (gtk:gtk-widget-show-all window))
```

Although written in C, GTK is object oriented. It uses a portable object system
called GObject. GObject classes are wrapped with corresponding Lisp classes.
You create a window by instantiating the `gtk:gtk-window` class.

The `gtk:gtk-box` is a packing widget. A `gtk:gtk-window` can only contain one
widget at a time. Packing widgets are you used to pack widgets together and
arrange them.

The `gtk:within-main-loop` macro does the work of setting up a GTK main loop.
The macro does some additional bookkeeping to run the GUI in a separate thread.
This is cool, because even after the window appears, you can still type
commands into the REPL to interact with the program (e.g. query the properties
of a widget). With most of the Lisp GUI libraries I've tried out, the GUI takes
over completely once it is launched, and you have to close the window before
being able to type commands into the REPL again.

Then add our packing widget to the window, and we're ready to launch our
window. Now let's add some more widgets to it.

```lisp
(defvar slider (make-instance 'gtk:gtk-scale
                              :orientation :horizontal
                              :draw-value nil
                              :width-request 200
                              :adjustment
                              (make-instance 'gtk:gtk-adjustment
                                             :value 440
                                             :lower 20
                                             :upper 20000
                                             :step-increment 1)))
(gtk:gtk-box-pack-start vbox slider)
```

The range of frequencies audible by humans is typically between 20 Hz and 20
KHz (we lose the ability to hear some of those higher frequencies as we age).
The [musical note A above middle
C](https://en.wikipedia.org/wiki/A440_(pitch_standard)) is 440 Hz. Since A4
serves as a general tuning standard, it seems like a sensible default, but if
you run the above in SBCL, this is what you'll see:

![Slider](../../screenshots/clcffigtk-linearslider.png?raw=true "Slider showing 440 using a linear scale")

The scale of 20 to 20,000 is so large that 440 doesn't appear to move the
slider at all. Ideally, 440 would fall about the middle of the slider. To
achieve this, let's use a logarithmic scale.

I found a [Stack Overflow
answer](https://stackoverflow.com/questions/846221/logarithmic-slider/846249#846249)
on how to map a slider to a logarithmic scale. The code given in the answer is
JavaScript, but it was easy enough to port to Common Lisp.

```lisp
; Scale used by slider
(define *min-position* 0)
(define *max-position* 2000)
; Range of frequencies
(define *min-frequency* 20)
(define *max-frequency* 20000)

; Logarithmic scale for frequency (so middle A [440] falls about in the middle)
; Adapted from https://stackoverflow.com/questions/846221/logarithmic-slider

(define min-freq (log *min-frequency*))
(define max-freq (log *max-frequency*))
(define frequency-scale (/ (- max-freq min-freq) (- *max-position* *min-position*)))
; Convert slider position to frequency
(define (position->frequency position)
  (inexact->exact (round (exp (+ min-freq (* frequency-scale (- position *min-position*)))))))
; Convert frequency to slider position
(define (frequency->position freq)
  (inexact->exact (round (/ (- (log freq) min-freq) (+ frequency-scale *min-position*)))))
```

I added some global parameters to the top of the script. The variable name
`*min-position*` is just a Lisp naming convention for global parameters. I came
up with the range of 0-2,000 by trial and error. It seemed to strike the best
balance between each step of the slider making a noticeable change to the
frequency while still allowing the user to narrow in on a specific frequency
with just the slider.

Then we create two functions: one that takes the position on the slider and
returns the frequency (`position->frequency`) and another that takes a
frequency and returns the position on the slider (`frequency-position`). Now
let's modify our slider to use `frequency->position` to convert the initial
`value` to a slider position using our logarithmic scale.

```lisp
(defvar slider (make-instance 'gtk:gtk-scale
                              :orientation :horizontal
                              :draw-value nil
                              :adjustment
                              (make-instance 'gtk:gtk-adjustment
                                             :value (frequency->position 440)
                                             :lower *min-position*
                                             :upper *max-position*
                                             :step-increment 1)))
```

Underneath the slider is a spin button showing the current frequency and
buttons to increase/decrease the frequency by one octave.

```lisp
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

(defvar frequency-box (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 25))
(defvar lower-button (make-instance 'gtk:gtk-button :label "<"))
(gtk:gtk-box-pack-start frequency-box lower-button :fill nil)
(defvar frequency-field (units-spin-button *min-frequency* *max-frequency* 440 "Hz"))
(gtk:gtk-box-pack-start frequency-box (gtk:gtk-widget-parent frequency-field) :fill nil)
(defvar higher-button (make-instance 'gtk:gtk-button :label ">"))
(gtk:gtk-box-pack-start frequency-box higher-button :fill nil)
(gtk:gtk-box-pack-start vbox frequency-box)
```

We can also use boxes to help with layout. I created a function that I can
reuse later to generate the spin button and label and pack them together in a
box.

At this point, we are starting to have a nice looking interface, but it doesn't
do anything. If you click the buttons or slide the slider, nothing happens.
Widgets emit signals, and callback functions can be connected to these signals.
If we connect a callback function to the `value-changed` signal of the slider's
adjustment object, that function will be called each time the slider is moved.

```lisp
; Link slider to text field display of frequency
(defun adjust-frequency (adjustment)
  (setf (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field))
        (position->frequency (gtk:gtk-adjustment-value adjustment))))
(defun adjust-slider (adjustment)
  (setf (gtk:gtk-adjustment-value (gtk:gtk-range-adjustment slider))
        (frequency->position (gtk:gtk-adjustment-value adjustment))))
```

The arguments a callback function takes is dependent on the signal being
handled. Wire these functions up to the widgets:

```lisp
(gobject:g-signal-connect (gtk:gtk-range-adjustment slider) "value-changed" #'adjust-frequency)
(gobject:g-signal-connect (gtk:gtk-spin-button-adjustment frequency-field) "value-changed" #'adjust-slider)
```

Wire the buttons up to callback functions called `decrease-octave` and
`increase-octave`. An [octave](https://en.wikipedia.org/wiki/Octave) is "the
interval between one musical pitch and another with double its frequency."

```lisp
; Buttons increase and decrease frequency by one octave
(defun set-frequency (freq)
  (setf (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field)) freq))
(defun adjust-octave (modifier)
  (set-frequency (* (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field)) modifier)))
(defun decrease-octave (widget) (adjust-octave 0.5))
(defun increase-octave (widget) (adjust-octave 2))

(gobject:g-signal-connect lower-button "clicked" #'decrease-octave)
(gobject:g-signal-connect higher-button "clicked" #'increase-octave)
```

CONTINUE REWRITING RACKET TUTORIAL for SBCL/CL-CFFI-GTK HERE

Let's use this `number-field%` again to create a field to specify the duration
of the beep in milliseconds:

```racket
(define control-pane (new horizontal-pane% [parent frame]
                                           [border 25]
                                           [spacing 25]))
(define duration-pane (new horizontal-pane% [parent control-pane]))
(define duration-field (new number-field% [label "Duration "]
                                          [parent duration-pane]
                                          [min-value 1]
                                          [max-value 600000] ; 10 minutes
                                          [init-value "200"]
                                          [min-width 120]))
```

Frequency is rather abstract. Let's also give the user the ability to select a
musical note. We can store the corresponding frequencies for A4-G4 in a hash
table.

```racket
; Notes -> frequency (middle A-G [A4-G4])
; http://pages.mtu.edu/~suits/notefreqs.html
(define notes (hash "A" 440.00
                    "B" 493.88
                    "C" 261.63
                    "D" 293.66
                    "E" 329.63
                    "F" 349.23
                    "G" 292.00))
```

We'll give the user a drop-down menu. Whenever a note is selected from the
drop-down menu, we'll look up the frequency in the hash table and set it using
the `set-frequency` helper function we created for the octave buttons.

```racket
; Set frequency to specific note
(define (set-note choice event)
  (set-frequency (hash-ref notes (send choice get-string-selection))))
(define note (new choice% [label "♪ "]
                          [choices '("A" "B" "C" "D" "E" "F" "G")]
                          [parent control-pane]
                          [callback set-note]))
```

Finally, let's make some noise.

```racket
(require rsound)

; Generate a tone using RSound
; Explicitly set RSound sample rate in case differs by platform/version
(default-sample-rate 44100)
(define (generate-tone button event)
  (play (make-tone (string->number (send frequency-field get-value))
                   0.5
                   ; Duration in samples at sample rate of 44.1 kHz
                   (inexact->exact (* 44.1 (string->number (send duration-field get-value)))))))
```

We'll use the Racket [RSound](https://docs.racket-lang.org/rsound/index.html)
package to generate the tone. This package isn't bundled with Racket, but you
can install it with the `raco` utility that comes with Racket (`raco pkg
install rsound`). Wire this up to a button between the duration and note
selector, and you're ready to make some noise.

```racket
(define play-button (new button% [parent control-pane]
                                 [label "Play"]
                                 [callback generate-tone]))
```
