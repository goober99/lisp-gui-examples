PS/Tk stands for a portable Scheme interface to the Tk GUI toolkit. It has a
rich history going all the way back to Scheme_wish by Sven Hartrumpf in 1997.
Wolf-Dieter Busch created a Chicken port called Chicken/Tk in 2004. It took on
its current name when Nils M Holm stripped it of Chicken-isms to make it
portable amongst Scheme implementations in 2006.

If you've ever tried to write portable Scheme, you know that, except for the
most trivial of programs, it is much easier said than done. Holm's `pstk.scm`
had a configurable section titled `NON-PORTABLE` that you had to configure for
your chosen implementation. It came full circle and was repackaged as a Chicken
egg.

Chicken is a popular Scheme implementation that compiles Scheme to C. Eggs are
Chicken-specific extenstion libraries that are stored in a centralized
repository (like CPAN but for Chicken Scheme). Instead of building yet another
calculator, let's build a GUI for generating a tone.

![Screenshot](../../screenshots/racket.png?raw=true "Example screenshot")

You'll need Chicken installed. It's available in the repositories of most Linux
distros. PS/Tk interfaces with Tk, not with C library bindings, but with a
named pipe to `tclsh8.6`. The TCL package in most Linux distros will provide
this. For Debian, I did `sudo apt install chicken-bin tcl tk`. Once Chicken is
installed, you can use the `chicken-install` utility that comes with it to
install the PS/Tk egg.

```console
$ chicken-install -sudo pstk
```

When you think of Tk, you may think of something that looks like this:

![Screenshot](../../screenshots/tk-old.png?raw=true "Legacy Tk open dialog")

Tk has come a long way in recent years. Tcl/Tk 8.5 and later comes with a new
set of widgets built in called Tile or Ttk that can be themed. These widgets
are available alongside the classic widgets, so you have to explicitly tell
your app to use Ttk or else it will end up looking like it was designed for a
1980s Unix workstation.

```scheme
(import pstk)

(tk-start)
(ttk-map-widgets 'all) ; Use the Ttk widget set

(tk/wm 'title tk "Bleep")

(tk-event-loop)
```

All PS/Tk function names begin with `tk/` or `tk-` (or `ttk/` and `ttk-` for
the few Ttk-specific functions). The [doc directory] in the PS/Tk GitHub repo
unfortunately has not been updated since this convention was adopted. One
example from the docs is `start-tk` which is now `tk-start`.

The `ttk-map-widgets` function is what tells Tk to use the Ttk widgets instead
of the classic widgets. Tk comes with a few built-in themes. The default themes
on Windows and macOS supposedly do a decent job of approximating the look of
native widgets on those platforms. I don't use either of those platforms, so I
can't verify this first hand. For some reason, the default theme on Linux is
vaguely Windows 95ish. It comes with a built-in theme called
[clam](https://wiki.tcl-lang.org/page/ttk%3A%3Atheme%3A%3Aclam) that is
supposed to provide "a look somewhat like a Linux application". You can set
this theme with `(ttk/set-theme "clam")`, but it's really not that much of an
improvement.

Ideally, something like [gtkTtk](https://github.com/Geballin/gtkTtk) that has
GTK do the actual drawing would be integrated into Tcl/Tk and become the
default on Linux. In the meantime, there are [third party
themes](https://ttkthemes.readthedocs.io/en/latest/themes.html) that imitate
the look and feel of the most popular GTK and Qt themes. I use MATE with the
Arc GTK theme, so I went with the Arc theme. There was even a Debian package
for it (`sudo apt install tcl-ttkthemes`). We can then [apply the theme system
wide](https://blog.serindu.com/2019/03/07/applying-tk-themes-to-git-gui/)
(`echo '*TkTheme: arc' | xrdb -merge -`), so that all Tk apps such as git-gui
also inherit the theme. It is probably better to give your Linux users
instructions on how to install their own theme instead of hard coding one with
`ttk/set-theme`, so they can choose one that matches their system theme (KDE
users might pick Breeze while Ubuntu users might opt for Yaru). The screenshots
in this tutorial use the Arc theme.

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
