The GTKlos extension provides bindings to GTK for the STklos Scheme
implementation. One of the selling points of STklos is easy connection to GTK.
Instead of building yet another calculator, let's build a GUI for generating a
tone.

![Screenshot](../../screenshots/clcffigtk.png?raw=true "Example screenshot")

You'll need STklos installed. If it's not available from your distro's repo,
it's easy to compile. Depending on your desktop environment, you probably
already have GTK installed (even if you use KDE, it's highly likely you already
have GTK installed). Presently, STklos only runs on Linux and macOS, so it's not
very cross-platform. More useful for slapping a GUI on any Scheme Unix utilities
you've written. You'll also need to compile the GTKlos extension. It's included
in the source tree of STklos, so after compiling STklos, you can `cd` into the
GTKlos subdirectory and compile it. [Download](https://stklos.net/download.html)
the most recent version of STklos, then extract, and compile:

```console
$ tar -xvf stklos-*.tar.gz
$ cd stklos*/
$ ./configure
$ make
$ sudo make install
$ cd extensions/gtklos
$ export STKLOS_GTK_DIR=/usr/lib/x86_64-linux-gnu
$ make
$ ./run-demos
```

If the demos work, go ahead and install (`sudo make install`).

```scheme
; Load GTklos
(require "gtklos")
(import GTKLOS)

; Main window
(define window (make <vwindow> #:title "Bleep" #:border-width 25))

(gtk-main)
```

You can comment out `gtk-main`, load the file in the REPL with `(load
"bleep.scm")`, and start the GTK event loop with `start-interactive-gtk`
instead. This will return control to the REPL after displaying the window. This
way you can interact with the GUI from the REPL. This can be helpful during
development.

STklos has an object system based on CLOS. You create a window by instantiating
the `<window>` class (or `<vwindow>` for a window with an embedded vbox).
Identifiers enclosed with angle brackets (like HTML tags) is the STklos naming
convention for classes. Documentation for GTKlos is almost non-existent. The
best way to find out what widgets are available it to browse the
[source](https://github.com/egallesio/STklos/tree/master/extensions/gtklos/lib/stklos/widgets)
for the widgets. Then you can do `(describe <class>)` (e.g. `(describe
<window>)`) to get a list of slots and methods supported by that class. Now
let's add some additional widgets between creating the window and starting the
GTK main loop.

```scheme
(define slider (make <scale> #:parent window
                             #:orientation 'horizontal
                             #:draw-value #f
                             #:from 20
                             #:to 20000
                             #:value 440))
```

The range of frequencies audible by humans is typically between 20 Hz and 20
KHz (we lose the ability to hear some of those higher frequencies as we age).
The [musical note A above middle
C](https://en.wikipedia.org/wiki/A440_(pitch_standard)) is 440 Hz. Since A4
serves as a general tuning standard, it seems like a sensible default, but if
you run the above in STklos, this is what you'll see:

![Slider](../../screenshots/gtklos-linearslider.png?raw=true "Slider showing 440 using a linear scale")

The scale of 20 to 20,000 is so large that 440 doesn't appear to move the
slider at all. Ideally, 440 would fall about the middle of the slider. To
achieve this, let's use a logarithmic scale.

I found a [Stack Overflow
answer](https://stackoverflow.com/questions/846221/logarithmic-slider/846249#846249)
on how to map a slider to a logarithmic scale. The code given in the answer is
JavaScript, but it was easy enough to port to Scheme.

```scheme
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
  (round (exp (+ min-freq (* frequency-scale (- position *min-position*))))))
; Convert frequency to slider position
(define (frequency->position freq)
  (round (/ (- (log freq) min-freq) (+ frequency-scale *min-position*))))
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

```scheme
(define slider (make <scale> #:parent window
                             #:orientation 'horizontal
                             #:draw-value #f
                             #:from *min-position*
                             #:to *max-position*
                             #:value (frequency->position 440)))
```

Underneath the slider is a text field showing the current frequency and buttons
to increase/decrease the frequency by one octave.

```scheme
(define frequency-pane (make <hbox> #:parent window #:spacing 25))
(define lower-button (make <button> #:parent frequency-pane #:text "<"))
(define frequency-control (make <hbox> #:parent frequency-pane #:spacing 10))
(define frequency-field (make <entry> #:parent frequency-control #:value "440"))
(define frequency-label (make <label> #:parent frequency-control #:text "Hz"))
(define higher-button (make <button> #:parent frequency-pane #:text ">"))
```

The `<hbox>` is an invisible widget that helps with layout. At this point, we
are starting to have a nice looking interface, but it doesn't do anything. If
you click the buttons or slide the slider, nothing happens. The widgets have a
`command` slot that wires the widgets up to a function. If we add a command to
the slider, that command will be called each time the slider is moved.

```scheme
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
(defvar notes '(("A" . 440.00)
                ("B" . 493.88)
                ("C" . 261.63)
                ("D" . 293.66)
                ("E" . 329.63)
                ("F" . 349.23)
                ("G" . 292.00)))
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
