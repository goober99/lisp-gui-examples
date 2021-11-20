The cl-cffi-gtk library provides Common Lisp bindings to GTK. The library is
developed with SBCL but should also work with Clozure CL and CLISP. For this
tutorial, I'll be using SBCL. Instead of building yet another calculator, let's
build a GUI for generating a tone.

![Screenshot](../../screenshots/clcffigtk.png?raw=true "Example screenshot")

You'll need SBCL installed. It's available in the repositories of most Linux
distros, so just install it from your distro's repo. Depending on your destkop
environment, you probably already have GTK installed (even if you use KDE, it's
highly likely you already have GTK installed). If you're on Windows or macOS,
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

(defvar frequency-field (units-spin-button *min-frequency* *max-frequency* 440 "Hz" :digits 2))

(defvar lower-button (make-instance 'gtk:gtk-button :label "<"))
(defvar higher-button (make-instance 'gtk:gtk-button :label ">"))

(defvar frequency-box (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 25))
(gtk:gtk-box-pack-start frequency-box lower-button :fill nil)
(gtk:gtk-box-pack-start frequency-box (gtk:gtk-widget-parent frequency-field) :fill nil)
(gtk:gtk-box-pack-start frequency-box higher-button :fill nil)
(gtk:gtk-box-pack-start vbox frequency-box)
```

We can also use boxes to help with layout. I created a function that I can
reuse later to generate the spin button and label and pack them together in a
box.

At this point, we are starting to have a nice looking interface, but it doesn't
do anything. If you click the buttons or slide the slider, nothing happens.
Widgets emit signals, and callback functions can be connected to these signals.
If we connect a callback function to the `change-value` signal of the slider,
that function will be called whenever the slider is moved. The arguments a
callback function takes are dependent on the signal being handled. The
adjustment object of the spin button has a `value-changed` signal.

```lisp
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
(defun decrease-octave (widget) (declare (ignore widget)) (adjust-octave 0.5))
(defun increase-octave (widget) (declare (ignore widget)) (adjust-octave 2))

(gobject:g-signal-connect lower-button "clicked" #'decrease-octave)
(gobject:g-signal-connect higher-button "clicked" #'increase-octave)
```

We'll reuse the `units-spin-button` function we created to create a field to
specify the duration of the beep in millseconds:

```lisp
(defvar control-box (make-instance 'gtk:gtk-box :orientation :horizontal :spacing 25))
(defvar duration-field (units-spin-button 1 600000 200 "ms"))
(gtk:gtk-box-pack-start control-box (gtk:gtk-widget-parent duration-field) :fill nil)
(gtk:gtk-box-pack-start vbox control-box)
```

Frequency is rather abstract. Let's also give the user the ability to select a
musical note. We can store the corresponding frequencies for A4-G4 in an
association list.

```lisp
; Notes -> frequency (middle A-G [A4-G4])
; http://pages.mtu.edu/~suits/notefreqs.html
(define notes '(("A" 440.00)
                ("B" 493.88)
                ("C" 261.63)
                ("D" 293.66)
                ("E" 329.63)
                ("F" 349.23)
                ("G" 292.00)))
```

We'll give the user a drop-down menu. Whenever a note is selected from the
drop-down menu, we'll look up the frequency in the association list and set it
using the `set-frequency` helper function we created for the octave buttons.

```lisp
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
    (let ((value (gtk:gtk-combo-box-text-get-active-text object)))
      (set-frequency (cdr (assoc value notes :test 'equal))))))
; Pack the combo box
(gtk:gtk-box-pack-start control-box note-box :fill nil)
(gtk:gtk-box-pack-start vbox control-box)
```

Finally, let's make some noise.

```lisp
(ql:quickload :cl-portaudio)

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
```

We'll use [Common Lisp bindings to
PortAudio](https://github.com/filonenko-mikhail/cl-portaudio) to generate the
tone. This can be loaded with [Quicklisp](https://www.quicklisp.org/).

CL-PortAudio comes with a couple of helpful macros that makes initializing
PortAudio and starting a stream simple. The `with-audio` macro executes body
within a PortAudio initialize/terminate environment. The
`with-default-audio-stream` macro executes body with an opened and started
stream and shuts down the stream after it is done.

Then you just feed PortAudio arrays of samples, `:frames-per-buffer` at a time.
I initiated `with-default-audio-stream` with one channel, so the array is just
a single-dimensional array of floating point numbers. If you were producing
stereo sound, you would generate a two-dimensional array. The [basic formula
for a sine wave](http://pld.cs.luc.edu/telecom/mnotes/digitized_sound.html) is
A sin(2πft) where *A* is amplitude, *f* is frequency, and *t* is time:

```lisp
(* amplitude (sin (* 2 pi frequency time)))
```

Wire this up to a button between the duration and note selector, and you're
ready to make some noise.

```lisp
(defvar play-button (make-instance 'gtk:gtk-button :label "Play"))
(gobject:g-signal-connect play-button "clicked" (lambda (widget)
  (declare (ignore widget))
  (generate-tone (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment frequency-field))
                 (gtk:gtk-adjustment-value (gtk:gtk-spin-button-adjustment duration-field)))))
(gtk:gtk-box-pack-start control-box play-button :fill nil)
```
