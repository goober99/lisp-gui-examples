IUP is a cross-platform GUI toolkit that uses native controls. It uses GTK on
Linux and, well, GTK on macOS too. According to the roadmap, there is a macOS
native driver in the works, but it looks like one of those perpetually "coming
soon" features. There are bindings for various programming languages including
a Chicken egg. Instead of building yet another calculator, let's build a GUI
for generating a tone.

![Screenshot](../../screenshots/pstk.png?raw=true "Example screenshot")

You'll need Chicken Scheme installed. It's available in the repositories of
most Linux distros. You'll also need the IUP libraries installed before you can
install the IUP egg. Unfortunately, IUP doesn't appear to be packaged for many
Linux distros. Fortunately, there are pre-compiled binaries available for
download on the IUP website. They are packaged as a tarball. Each tarball
includes shell scripts for installing the libraries (moving them to system-wide
locations for which `sudo` may be required depending on your distro). In
addition to [IUP](https://www.tecgraf.puc-rio.br/iup/en/download.html), you
also need to download two other libraries also developed by Tecgraf Institute
at PUC-Rio called [IM](https://www.tecgraf.puc-rio.br/im/en/download.html) and
[CD](https://www.tecgraf.puc-rio.br/cd/en/download.html). For Linux, there are
different downloads for different kernel versions. These appear to correspond
to versions of the Linux kernel that came with different Ubuntu releases. I use
Debian Sid, which has a newer kernel than any Ubuntu release, so I just
downloaded the package for the most recent kernel available, and it worked
fine. Position yourself in the directory where you downloaded the tarballs and:

```console
$ mkdir iup
$ tar -xpvf iup-*_lib.tar.gz -C iup
$ sudo iup/install
$ sudo iup//install_dev
$ cd ~/Development/iup/ftgl/lib/Linux*/*
$ sudo cp iup/ftgl/lib/Linux*/* /usr/lib64
$ mkdir im
$ tar -xpvf im-*_lib.tar.gz -C im
$ sudo im/install
$ sudo im/install_dev
$ mkdir cd
$ tar -xpvf cd-*_lib.tar.gz -C cd
$ sudo cd/install
$ sudo cd/install_dev
```

The above scripts install the libraries in non-standard paths on Debian, and
`chicken-install` is unable to find them. You can use the `CSC_OPTIONS`
environment variable to tell `chicken-install` where to find these files.

```console
$ CSC_OPTIONS='-I/usr/include/iup -I/usr/include/im -I/usr/include/cd -L/usr/lib64' chicken-install -sudo iup
```

If using IUP from its native C, attribute setting is a separate step from
object construction. The Scheme bindings add a little syntactic sugar that
allow specifying your entire UI as an S-expression. Attribute names are
keywords which can be passed directly to widget constructors along with child
widgets. You can look up what attributes a widget accepts in the original C
documentation. In Chicken, keywords can be written with either a leading or
trailing colon. For the examples in this tutorial, a trailing colon is used.

```scheme
(import iup)

(define dlg
  (dialog
    title: "Bleep"
    (vbox
      alignment: 'ACENTER
      gap: 25
      margin: '25x25)))

(show dlg)
(main-loop)
(destroy! dlg)
(exit 0)
```

IUP convention is to use "Yes" and "No" as boolean values. In Scheme you can
use the 'Yes and 'No symbols.

We set the window title with `tk/wm` and start the event loop with
`tk-event-loop`. We now have an empty window. Now let's add some widgets to
this window.

```scheme
(define slider (tk 'create-widget 'scale 'from: 20 'to: 20000))
(slider 'set 440)
(tk/grid slider 'row: 0 'columnspan: 3 'sticky: 'ew 'padx: 20 'pady: 20)
```

Widgets are organized hierarchically. This is done by invoking a parent widget
with the sub-command `create-widget`. PS/Tk associates a widget named `tk` with
the top-level window, so most widgets will start as a call to `tk` (e.g. `(tk
'create-widget 'label 'text: "Hello, World!")`). Options are quoted and get a
trailing colon (e.g. `'text: "Hello, World!"`).

Creating a widget returns a Scheme function. If you give this function a name,
you can call it with sub-commands such as `configure`, `get`, and `set`. Just
creating a widget doesn't make it appear on screen. For that you need a
geometry manager, of which Tk has three: the packer, the gridder, and the
placer (`tk/pack`, `tk/grid`, and `tk/place` in Scheme, respectively).

The range of frequencies audible by humans is typically between 20 Hz and 20
KHz (we lose the ability to hear some of those higher frequencies as we age).
The [musical note A above middle
C](https://en.wikipedia.org/wiki/A440_(pitch_standard)) is 440 Hz. Since A4
serves as a general tuning standard, it seems like a sensible default, but if
you run the above in Chicken, this is what you'll see:

![Slider](../../screenshots/pstk-linearslider.png?raw=true "Slider showing 440 using a linear scale")

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
let's set the initial position of our slider with the `frequency->position`
function:

```scheme
(define slider (tk 'create-widget 'scale 'from: *min-position* 'to: *max-position*))
(slider 'configure 'value: (frequency->position 440))
```

Underneath the slider is a spin box showing the current frequency and buttons
to increase/decrease the frequency by one octave.

```scheme
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
    (tk/pack spinbox label 'side: 'left 'padx: 2)
    (values container spinbox)))

(define lower-button (tk 'create-widget 'button 'text: "<"))
(define-values (frequency-ext frequency-int)
  (units-spinbox *min-frequency* *max-frequency* 440 "Hz"))
(define higher-button (tk 'create-widget 'button 'text: ">"))

(tk/grid lower-button 'row: 1 'column: 0 'padx: 20 'pady: 20)
(tk/grid frequency-ext 'row: 1 'column: 1 'padx: 20 'pady: 20)
(tk/grid higher-button 'row: 1 'column: 2 'padx: 20 'pady: 20)
```

The frame widget is an invisible widget that helps with layout. Since all I
need to arrange within the frame is a spin box and a label, I used `tk/pack` to
`pack` them side by side. The frame is then organized in a `grid` with the rest
of the widgets. I created a function that I can reuse later to generate the
spin box, label, and frame all together. At this point, we are starting to have
a nice looking interface, but it doesn't do anything. If you click the buttons
or slide the slider, nothing happens. The widgets have a `command` option that
wires the widget up to a function. If we add a command to the slider, that
command will be called each time the slider is moved.

```scheme
(define slider (tk 'create-widget 'scale 'from: *min-position* 'to: *max-position*
                   'command: (lambda (x) (frequency-int 'set (position->frequency x)))))
```

The command for the slider takes one argument that indicates the new value of
the slider. The spin box does have a `command` option, but the command is only
called when the value is changed by clicking the up or down arrow, not when the
value is changed by other means such as typing a frequency into the field. Tk
has a `bind` command (the Scheme `tk/bind` function) that allows binding
functions to an event on a widget. We'll bind our callback to the `KeyRelase`
event. The `tk/bind` function takes up to three arguments. The first is the
widget to bind to (or a tag created with `tk/bindtags` to apply the binding to
multiple widgets). The second is the event pattern. The event pattern is
surrounded by angle brackets and can specify modifiers, event types, and more.
You can find detailed documentation on the event pattern in the [Tcl/Tk
documentation](https://www.tcl.tk/man/tcl8.6/TkCmd/bind.htm). The third is a
lambda expression to associate with the event.

```scheme
(tk/bind frequency-int '<KeyRelease> (lambda ()
  ; If frequency value is a valid number, set slider to current value
  (let ((numified (string->number (frequency-int 'get))))
    (when numified
      (slider 'configure 'value: (frequency->position numified))))))
```

Wire the buttons up to callback functions called `decrease-octave` and
`increase-octave`. An [octave](https://en.wikipedia.org/wiki/Octave) is "the
interval between one musical pitch and another with double its frequency."

```scheme
; Set frequency slider and display
(define (set-frequency freq)
  (when (and (>= freq *min-frequency*) (<= freq *max-frequency*))
    (slider 'configure 'value: (frequency->position freq))
    (frequency-int 'set freq)))

; Buttons increase and decrease frequency by one octave
(define (adjust-octave modifier)
  (set-frequency (* (string->number (frequency-int 'get)) modifier)))
(define (decrease-octave) (adjust-octave 0.5))
(define (increase-octave) (adjust-octave 2))
```

If you slide the slider, the text field updates accordingly. If you type a
number in the text field, the slider updates accordingly. All good, right? What
if a user (and you know they will) enters a number higher than 20,000 or a
letter?

Let's extend the function that returns our labeled spin box to bind a
validation function to the `FocusOut` event on the spin box. The spin box does
have a `validatecommand` option, but I wasn't able to get it working. I looked
through the examples that have come with the various variations of PS/Tk and
couldn't find a single example of a spin box with a `validatecommand`. I even
looked at the source code for
[Bintracker](https://github.com/bintracker/bintracker/), a chiptune audio
workstation written in Chicken Scheme with a PS/Tk GUI and developed by the
current maintainer of the PS/Tk egg. Even it binds a `validate-new-value`
function to the `Return` and `FocusOut` events of the spin box rather than
using `validatecommand`.

```scheme
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
```

We'll also use this function to create a field to specify the duration of the
beep in milliseconds:

```scheme
(define-values (duration-ext duration-int) (units-spinbox 1 600000 200 "ms"))
(tk/grid duration-ext 'row: 2 'column: 0 'padx: 20 'pady: 20)
```

Frequency is rather abstract. Let's also give the user the ability to select a
musical note. We can store the corresponding frequencies for A4-G4 in an
association list.

```scheme
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

```scheme
(define note-frame (tk 'create-widget 'frame))
(define note (note-frame 'create-widget 'combobox 'width: 2 'values: '("A" "B" "C" "D" "E" "F" "G")))
(tk/bind note '<<ComboboxSelected>> (lambda () (set-frequency (cadr (assoc (note 'get) notes)))))
(define note-label (note-frame 'create-widget 'label 'text: "♪"))
(tk/pack note-label note 'side: 'left 'padx: 2)
(tk/grid note-frame 'row: 2 'column: 2 'padx: 20 'pady: 20)
```

Now, let's make some noise. There are Chicken Scheme
[bindings](http://wiki.call-cc.org/eggref/5/allegro) to the
[Allegro](https://en.wikipedia.org/wiki/Allegro_(software_library)) library.
Allegro is a library primarily used by games for cross-platform graphics, input
devices, and more. What we're interested in is the audio addon that can be used
to generate a tone with a sine wave. You'll need to install the Allegro
library. Make sure you also install the header files. In some Linux distros,
these are split into a separate package (e.g. `liballegro5-dev` on Debian).
Also, install the Allegro egg (`chicken-install -sudo allegro`). I added the
following lines near the top to import the Allegro bindings (and the chicken
memory module, which we'll also use) and initialize Allegro.

````scheme
(import (prefix allegro "al:"))
(import (chicken memory))

(define +pi+ 3.141592)

; Initialize Allegro and audio addon
(unless (al:init) (print "Could not initialize Allegro."))
(unless (al:audio-addon-install) (print "Could not initialize sound."))
(al:reserve-samples 0)
````

The Allegro egg is accompanied by a couple of examples but no examples showing
the use of the audio addon. The Allegro library itself comes with an [example
showing how to generate a saw
wave](https://github.com/liballeg/allegro5/blob/master/examples/ex_saw.c), but
being a C library, the example is, of course, in C. I [ported that example to
Scheme](https://github.com/goober99/lisp-gui-examples/blob/master/examples/pstk/saw.scm).
I would have contributed the example back to the Allegro egg, but the repo is
marked as "archived by the owner" and read-only on GitHub. I've included the
example in the repo alongside the rest of the code for this tutorial in case
someone finds it useful.

Allegro is very low-level. You create an audio `stream`. In this case, the
stream buffers eight fragments of 1,024 samples each at a frequency (often
called sampling rate) of 44,100 Hz (the sampling rate of an audio CD), which
means there are 44,100 samples per second. Each sample is a 32-bit float (what
is called the bit depth of the audio), and we only have one channel to keep
things as simple as possible.

```scheme
; Generate a tone using Allegro
(define (generate-tone frequency duration)
  (let* ((samples-per-buffer 1024)
         (stream-frequency 44100)
         (amplitude 0.5)
         (stream (al:make-audio-stream 8 samples-per-buffer stream-frequency 'float32 'one))
         (queue (al:make-event-queue))
         (event (al:make-event)))

    (unless (al:audio-stream-attach-to-mixer! stream (al:default-mixer))
      (print "Could not attach stream to mixer."))
    (al:event-queue-register-source! queue (al:audio-stream-event-source stream))

    (let event-loop ((n 0))
      ; Grab and handle events
      (when (and (< n (/ (* (/ duration 1000) stream-frequency) samples-per-buffer))
                 (al:event-queue-wait! queue event))
        (case (al:event-type event) ('audio-stream-fragment
          (let ((buffer (al:audio-stream-fragment stream)))
            ; If the stream is not ready for new data, buffer will be null.
            (if (not buffer) (event-loop n) (begin
              (fill-buffer buffer n) ; Placeholder
              ; Repeat
              (event-loop (+ n 1)))))))))

    (al:audio-stream-drain stream)))
```

An event loop waits for the audio stream to ask for another buffer. Our job is
to fill that buffer with 1,024 32-bit floats at a time. In the code listing
above, this is done by `fill-buffer`. That was just a placeholder, so I could
break the code up into shorter, more easily explainable chunks. This is what
goes in the place of `(fill-buffer buffer n)`:

```scheme
(let ((adr (pointer->address buffer)))
  (let loop ((i 0))
    (when (< i samples-per-buffer)
      (let ((time (/ (+ (* samples-per-buffer n) i) stream-frequency)))
        ; al:audio-stream-fragment returns a C pointer. Use (chicken
        ; memory) module to operate on foreign pointer objects.
        ; Iterate over array four bytes at a time since 32-bit depth.
        (pointer-f32-set! (address->pointer (+ adr (* i 4)))
          (* amplitude (sin (* 2 +pi+ frequency time))))
        (loop (+ i 1)))))
  (unless (al:audio-stream-fragment-set! stream buffer)
    (print "Error setting stream fragment")))
```

The Allegro egg is a pretty thin wrapper of the Allegro library. The
`audio-stream-fragment` procedure in the egg just passes along the C pointer
that the corresponding `al_get_audio_stream_fragment` function from the C
library returns. It would have been nice if the egg had offered some Scheme
conveniences atop Allegro like allowing us to pass a Scheme list or array to
Allegro to provide the buffer of samples. Since it doesn't, we'll use the
chicken memory module to fill the C array starting at the C pointer returned by
`audio-stream-fragment`. We use `pointer->address` to get the address of the
pointer. A pointer refrences a byte of memory. We can reference the preceding
or following byte by subtracting or adding 1 to the address. Since we are
filling the array with 32-bit floats, and 32 bits is 4 bytes, we want to
increment the address by 4 each time. Then we can set the value of the current
location with pointer-f32-set!.

Then you just need to feed Allegro buffers of 1,024 samples at a time. The
[basic formula for a sine
wave](http://pld.cs.luc.edu/telecom/mnotes/digitized_sound.html) is A sin(2πft)
where *A* is amplitude, *f* is frequency, and *t* is time.

```scheme
(* amplitude (sin (* 2 +pi+ frequency time)))
```

Wire this up to a play button, and you're ready to make some noise.

```scheme
(define play-button (tk 'create-widget 'button 'text: "Play" 'command: (lambda ()
  (generate-tone (string->number (frequency-int 'get)) (string->number (duration-int 'get))))))
(tk/grid play-button 'row: 2 'column: 1 'padx: 20 'pady: 20)
```

Tk has been around a long time, and it shows. While it is stable and highly
portable, even with recent improvements, it just looks a little dated. At least
on Linux, none of the themes I tried really fit in. There were always
differences that made the Tk GUI stick out like a sore thumb. If you're
building an internal tool where it doesn't really matter how pretty it is, you
can get Tk to work with a variety of Schemes in a variety of places.
