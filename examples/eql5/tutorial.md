Embedded Qt5 Lisp (EQL5) is a Qt5 binding for Embedded Common Lisp (ECL). EQL5
is a bit different than most bindings. Instead of executing your Lisp source
with `ecl` and importing the bindings, you compile a new interpreter (`eql5`)
that combines both ECL and Qt5 bindings. Instead of building yet another
calculator, let's build a GUI for generating a tone.

![Screenshot](../../screenshots/lambdanative.png?raw=true "Example screenshot")

## Compiling EQL5

EQL5 doesn't appear to be in the repos of any major Linux distro. It is
available from AUR on Arch, but since I use Debian, I compiled it from source.
Don't worry. It's not as scary as it sounds.

* Clone the EQL5 Git repo. EQL5 doesn't appear to have releases or tags, so I
guess you just clone `master` and hope that it's in a workable state.

```bash
git clone https://gitlab.com/eql/EQL5.git
```

* Install dependencies. This was actually the hardest part. Not the installing.
That's just `apt install whatever` (or the package manager of your preferred
distro). There are hundreds of Qt packages on Debian, and it's not always clear
what Qt module corresponds to what Debian package (here's a
[https://askubuntu.com/a/577334](list) I found useful). If I got an unknown
module error during the `make` step, I would look the Qt module up in the list
on Ask Ubuntu and install the corresponding dev package. I ended up needing to
install libqt5svg5-dev, qml-module-qtquick2, qtbase5-dev, qtdeclarative5-dev,
qtmultimedia5-dev, qttools5-dev, and qtwebengine5-dev. This will get EQL5
compiled, but if you choose to build your GUI with QML, depending on what
`import` statements you use in your QML file, you'll need some additional
packages. To follow this tutorial, you'll also need
qml-module-qtquick-controls2, qml-module-qtquick-layouts,
qml-module-qtquick-window2. The other dependency you'll need is ECL, and I also
recommend installing `qml` (it is helpful if building your GUI with QML). You
can install all the dependencies needed on Debian/Ubuntu with:

```bash
sudo apt install ecl libqt5svg5-dev qml-module-qtquick2 qtbase5-dev qtdeclarative5-dev qtmultimedia5-dev qttools5-dev qtwebengine5-dev qml-module-qtquick-controls2 qml-module-qtquick-layouts qml-module-qtquick-window2 qml
```

* Navigate into the `src` subdirectory of the cloned EQL5 Git repo, compile,
and install.

```bash
cd EQL5/src
ecl -shell make
qmake -qt5 eql5.pro
make
sudo make install
```

* Now `eql5` should be available to run. Verify it with `eql5 -qgui`, which
will launch a GUI REPL.

## Building the GUI with QML

EQL5 supports both Qt Widgets and Qt Quick. They primarily differ in how you
define your UI. With Qt Widgets, you declare it procedurally.  With Qt Quick,
you use a declarative language called QML to define your UI. The underlying
implementation of the widgets also differ. Qt Widgets has been around a lot
longer and has a broader range of available widgets, but many of these widgets
are more desktop-oriented without touch components and not easily adapted for
touch. Qt Quick, on the other hand, has been developed from the beginning to
work well on touch screens (tablets, smartphones, etc.).

Qt is essentially maintaining two sets of widgets. It would make more sense to
me if QML was just an alternative way of specifying which widgets to use
instead of a completely new set of widgets, but I'm not a Qt maintainer or even
a C++ developer. I guess creating new widgets was easier than adapting the old
widgets to be touch friendly.

Qt Quick is sometimes criticized for having a foreign look on the desktop
(neither Qt Quick or Qt Widgets use native widgets, but Qt Widgets look more
native on the desktop). It also has a smaller set of available widgets. Native
look and feel is not important to me, and Qt Quick has plenty of widgets to
implement the example in this tutorial. Given a choice between defining a GUI
procedurally and declaratively, I'm going to choose the declarative option.

## Coding the App

If your interface will use text (such as labels on buttons), you must include a
`FONTS` file in your application subdirectory. I just copied the `FONTS` file
from one of the demos included with LambdaNative.

```bash
cd apps/bleep
cp /opt/lambdanative/apps/LineDrop/FONTS .
```

This is what the file looks like:

```
DejaVuSans.ttf 8 18,25 ascii
```

For a description of the file format, see the documentation of the file on the
[LambdaNative wiki](https://github.com/part-cw/lambdanative/wiki/FONTS).

Your application subdirectory will already contain a `main.scm`. This file
contains the basic skeleton for a GUI app (the black rectangle above):

```scheme
;; LambdaNative gui template

(define gui #f)

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))

    ;; initialize gui here

  )
;; events
  (lambda (t x y)
    (if (= t EVENT_KEYPRESS) (begin
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend) (terminate))
;; resume
  (lambda () (glgui-resume))
)

;; eof

```

I started by changing the comment at the top to

```scheme
;; bleep - GUI for generating a tone made with LambdaNative
```

The bulk of the skeleton consists of the [event
loop](https://github.com/part-cw/lambdanative/wiki/Index-of-Module-eventloop).
The `(main p1 p2 p3 p4 p5)` loop takes five functions as arguments:

| Parameter | Description
| --------- | -----------
| p1        | Function to be run before the main loop is initialized. This is where you setup the GUI.
| p2        | Main loop function, which is called constantly throughout the application's life. This is where you listen for events like key presses. Since most widgets take a callback, you shouldn't need to do much in this area.
| p3        | Function to be run when the application is to be terminated.
| p4        | Function, which is called when the application is suspended.
| p5        | Function, which is called when the application is resumed.

The functions supplied in the skeleton for p3, p4, and p5 should be sufficient
for most applications. We won't need to touch them.

```scheme
(make-window 540 360)
(glgui-orientation-set! GUI_LANDSCAPE)
```

I started by changing the dimensions and orientation of the window. Now let's
add some widgets to that initialization `lambda`. I copy and pasted the example
from the bottom of the [slider documentation
page](https://github.com/part-cw/lambdanative/wiki/glgui-slider).

```scheme
(set! sl (glgui-slider gui 20 20 280 60 1 5 #f White White Orange Black num_25.fnt num_20.fnt #f White))
(glgui-widget-set! gui sl 'showvalue #t)
```

If you are familiar with Scheme, that `set!` probably made you pause. Too many
exclamation marks in my Scheme code always make me nervous. I immediately start
wondering if there is a better way to write the code. As it should. Side
effects should be avoided when possible. I tried changing the `set!` to
`define` and got the following error when recompiling:

```
*** ERROR IN "/home/matthew/lambdanative/apps/bleep/main.scm"@97.5 -- Ill-placed 'define'
```

Gambit (the underlying Scheme implementation used by LambdaNative) only allows
`define`s at the beginning of a `lambda` body. This actually conforms with the
R[5-7]RS specs, but I'm used to Scheme implementations (such as Racket,
Chicken, and MIT/GNU Scheme) that allow `define` anywhere in a `lambda` body.
All the LambdaNative examples and demos use `set!`, so I used it as well.

We must specify the color for several elements of the slider. LambdaNative
doesn't use native widgets but draws its own widgets with OpenGL. I googled
"color schemes" for inspiration and specified a few colors at the top of the
script above the `(main)` loop that I could reference throughout the program.

```scheme
;; UI color palette
(define *background-color* (color-rgb 26 26 29))
(define *foreground-color* (color-rgb 195 7 63))
(define *accent-color* (color-rgb 111 34 50))
(define *text-color* (color-rgb 255 255 255))
;; Scale used by slider
(define *min-position* 0)
(define *max-position* 2000)
;; Range of frequencies
(define *min-frequency* 20)
(define *max-frequency* 20000)
```

The variable name `*background-color*` is just a Lisp naming convention for
global parameters. LambdaNative provides `(color-rgb r g b)` for creating
colors. I also defined variables for the scale used by the slider and the range
of frequencies accepted by beep.

We also need to specify the position and size of the slider. Both are specified
in pixels. There aren't percentages or other scalable units you may be familiar
with from CSS. There are functions to get the width and height of the window,
so you could code the math to make a widget 80% the width of the window. Since
we're dealing with a simple example with hard-coded window dimensions, I just
hard-coded the position and size as well. Note that you specify the position
along the y-axis as pixels from the bottom of the window. This seemed counter
intuitive to me, and I continually caught myself trying to specify pixels from
the top of the window.

```scheme
;; Background color
(let ((w (glgui-width-get))
      (h (glgui-height-get)))
(glgui-box gui 0 0 w h *background-color*))

;; Frequency slider
(set! slider (glgui-slider gui 20 280 500 60 *min-position* *max-position* #f White *foreground-color* *accent-color* #f ascii_18.fnt ascii_18.fnt #f White))
(glgui-widget-set! gui slider 'showlabels #f)
```

I set a background color for the entire window. The only way I could find to do
this was to create a `glgui-box` the size of the entire window and set the
color of the box. I also renamed the variable from `sl` to `slider`.
LambdaNative has the tendency to use short, non-descriptive variable names
throughout its examples and documentation. I prefer to use more descriptive
variable names. Replace the fonts in the example slider code with the fonts we
specified in the `FONTS` file. I also disabled the slider labels.

The range of frequencies audible by humans is typically between 20 Hz and 20
KHz (we lose the ability to hear some of those higher frequencies as we age).
The [musical note A above middle
C](https://en.wikipedia.org/wiki/A440_(pitch_standard)) is 440 Hz. Since A4
serves as a general tuning standard, it seems like a sensible default.

The scale of 20 to 20,000 is so large that 440 wouldn't appear to move the
slider at all. Ideally, 440 would fall about the middle of the slider. To
achieve this, let's use a logarithmic scale.

I found a [Stack Overflow
answer](https://stackoverflow.com/questions/846221/logarithmic-slider/846249#846249)
on how to map a slider to a logarithmic scale. The code given in the answer is
JavaScript, but it was easy enough to port to Scheme.

```scheme
;; Logarithmic scale for frequency (so middle A [440] falls about in the middle)
;; Adapted from https://stackoverflow.com/questions/846221/logarithmic-slider

(define min-freq (log *min-frequency*))
(define max-freq (log *max-frequency*))
(define frequency-scale (/ (- max-freq min-freq) (- *max-position* *min-position*)))
;; Convert slider position to frequency
(define (position->frequency position)
  (inexact->exact (round (exp (+ min-freq (* frequency-scale (- position *min-position*)))))))
;; Convert frequency to slider position
(define (frequency->position freq) (/ (- (log freq) min-freq) (+ frequency-scale *min-position*)))
```

I created two functions: one that takes the position on the slider and returns
the frequency (`position->frequency`) and another that takes a frequency and
returns the position on the slider (`frequency-position`). Now let's set the
initial position of our slider with the `frequency->position` function.

```scheme
(glgui-widget-set! gui slider 'value (frequency->position 440))
```

Underneath the slider is a text field showing the current frequency, buttons to
increase/decrease the frequency by one octave, and a play button.

```scheme
;; Frequency display
(set! frequency-field (glgui-inputlabel gui 210 230 80 30 "440" ascii_18.fnt *text-color* *foreground-color*))
(glgui-widget-set! gui frequency-field 'align GUI_ALIGNCENTER)
(set! frequency-label (glgui-label gui 290 230 40 30 "Hz" ascii_18.fnt *foreground-color* *accent-color*))
(glgui-widget-set! gui frequency-label 'align GUI_ALIGNCENTER)

;; Octave buttons
(set! lower-button (glgui-button-string gui 140 230 50 30 "<" ascii_18.fnt (lambda (g w t x y) #t)))
(set! higher-button (glgui-button-string gui 350 230 50 30 ">" ascii_18.fnt (lambda (g w t x y) #t)))

;; Play button
(set! play-button (glgui-button-string gui 230 125 80 50 "Play" ascii_25.fnt (lambda (g w t x y) #t)))
```

That last argument to `glgui-button-string` is a callback function. This is a
function that is called when the button is pressed. I'm just trying to get the
widgets layed out right now. I don't yet care about the function of the button,
so I used anonymous functions (lambdas) that don't do anything for now.

The buttons do come with some default styling, but you'll probably want to
tweak the look to fit your color scheme and UI design. We can use
`glgui-widget-set!` to set parameters of a widget. Buttons have various
parameters that can be set such as `'button-normal-color` and
`'button-selected-color`.

```scheme
(glgui-widget-set! gui play-button 'button-normal-color *foreground-color*)
(glgui-widget-set! gui play-button 'button-selected-color *accent-color*)
(glgui-widget-set! gui play-button 'solid-color #t)
(glgui-widget-set! gui play-button 'rounded #f)
```

That seems like a lot to type (or copy and paste) for each button. With CSS I'm
able to define a style for all buttons or apply a class to buttons. I used a
`for-each` loop to loop through all the buttons and apply the above styling:

```scheme
;; Style buttons
(for-each (lambda (button)
            (glgui-widget-set! gui button 'button-normal-color *foreground-color*)
            (glgui-widget-set! gui button 'button-selected-color *accent-color*)
            (glgui-widget-set! gui button 'solid-color #t)
            (glgui-widget-set! gui button 'rounded #f))
          (list lower-button higher-button play-button))
```

At this point, we are starting to have a nice looking interface, but it doesn't
do anything. If you click the buttons or slide the slider, nothing happens.
While the buttons take a callback function parameter, I couldn't find a way to
wire up the slider to a function. I read the [`glgui-slider` documentation
page](https://github.com/part-cw/lambdanative/wiki/glgui-slider) several times
searching for clues.

Finally, I resorted to looking at the source code for `glgui-slider`. Each of
the widget documentation pages link directly to their implementation in the
LambdaNative GitHub repo. I already mentioned that I ended up reading the
LambdaNative source more than I would have liked for debugging. Documentation
is one area where LambdaNative really could stand to improve. I scanned
`slider.scm` and discovered it had a `'callback` parameter. I created a
function that would set the frequency displayed in the `glgui-inputlabel` to
the one that corresponded to the position of the `glgui-slider`.

```scheme
;; Link slider to text field display of frequency
(define (adjust-frequency)
  (glgui-widget-set! gui frequency-field 'label (number->string
    (position->frequency (glgui-widget-get gui slider 'value)))))
```

and wired it up to the slider:

```scheme
(glgui-widget-set! gui slider 'callback (lambda (parent widget event x y) (adjust-frequency)))
```

A callback function takes five arguments. In the code examples in the
LambdaNative documentation, these always appeared as `(lambda (g w t x y))`.
These one-letter variables aren't very descriptive, and the arguments of the
callback functions don't appear to be documented. Through experimentation and
reading the source code and examples, I worked out the following:

| Parameter | Description
| --------- | -----------
| g         | The [G]UI the widget belongs to. I used the name `parent` for this variable in my callback functions.
| w         | The [w]idget that triggered the callback function. I used the name `widget` for this variable in my callback functions.
| t         | The [t]ype of event. I used the name `event` for this variable in my callback functions.
| x         | First argument of event (x coordinate in pixels, keyboard character, etc.)
| y         | Second argument of event (y coordinate in pixels, modifier flags, etc.)

The callback function is only called once the user releases the slider handle.
I want the user to get feedback as they drag the slider. You can write your own
event handling code in the `lambda` that forms the second parameter of
`(main)`. The generated skeleton already includes code to terminate the
application when the `Esc` key is pressed. I added some code to call
`adjust-frequency` when the slider handle is being dragged:

```scheme
;; events
  (lambda (t x y)
    (if (= t EVENT_KEYPRESS) (begin
      (if (= x EVENT_KEYESCAPE) (terminate))))
    ;; Also update frequency when dragging slider (callback is only on release)
    (if (and (glgui-widget-get gui slider 'downval) (= t EVENT_MOTION)) (adjust-frequency))
    (glgui-event gui t x y))
```

By looking at the implementation of `glgui-slider` in `slider.scm`, I noticed
that LambdaNative was setting a `'downval` parameter whenever the user was
holding down the mouse button on the slider handle. Whenever that parameter is
true, I listen for an `EVENT_MOTION` event to call `adjust-frequency`.

I replaced the anonymous lambdas in the octave button declarations with
callback functions called `decrease-octave` and `increase-octave`. An
[octave](https://en.wikipedia.org/wiki/Octave) is "the interval between one
musical pitch and another with double its frequency."

```scheme
;; Set frequency slider and display
(define (set-frequency freq)
  (glgui-widget-set! gui slider 'value (frequency->position freq))
  (glgui-widget-set! gui frequency-field 'label (number->string freq)))
;; Buttons increase and decrease frequency by one octave
(define (adjust-octave modifier)
  (let ((new-freq (* (string->number (glgui-widget-get gui frequency-field 'label)) modifier)))
    (if (and (>= new-freq *min-frequency*) (<= new-freq *max-frequency*)) (set-frequency new-freq))))
(define (decrease-octave parent widget event x y) (adjust-octave 0.5))
(define (increase-octave parent widget event x y) (adjust-octave 2))
```

The `'aftercharcb` callback of `glgui-inputlabel` is called after each
character is typed or deleted. We can use this to update the slider as a user
enters a frequency. What if a user (and you know they will) enters a number
higher than 20,000 or a letter? We need a function that will only allow numbers
within a given range.

```scheme
;; Only allow numbers within range of min-value and max-value
(define (num-only min-value max-value old-value)
  (lambda (parent widget)
    (let* ((current-value (glgui-widget-get parent widget 'label))
           (current-numified (string->number current-value)))
      (if (or (= (string-length current-value) 0) ; Allow field to be empty
              (and current-numified (>= current-numified min-value) (<= current-numified max-value)))
          (set! old-value current-value)
          (glgui-widget-set! parent widget 'label old-value)))))
```

If the user types a character that makes the value invalid, we want to revert
to the last known good value. To accomplish this, I used a closure to remember
the last known value. Many programming languages today have closures, but
Scheme practically invented them. A closure enables variables to be associated
with a function that persist through all the calls of the function.

Now we can wire the `glgui-inputlabel` callback up to these functions.

```scheme
(set! frequency-range (num-only *min-frequency* *max-frequency* (glgui-widget-get gui frequency-field 'label)))
(glgui-widget-set! gui frequency-field 'aftercharcb (lambda (parent widget event x y)
  (frequency-range parent widget)
  (let ((freq (string->number (glgui-widget-get parent widget 'label))))
    (if freq (glgui-widget-set! parent slider 'value (frequency->position freq))))))
```

We call the `num-only` closure specifying the allowed range and initial value
which returns a new function that can be used in the callback. After we make
sure there are no high jinks going on with the value using the function created
by the closure (`frequency-range`), we update the position of the slider using
the current value of the text field.

We can use the  `num-only` closure again to create a field to specify the
duration of the beep in milliseconds:

```scheme
;; General Controls
(glgui-label gui 20 40 80 30 "Duration" ascii_18.fnt *foreground-color*)
(set! duration-field (glgui-inputlabel gui 110 40 80 30 "200" ascii_18.fnt *text-color* *foreground-color*))
(glgui-widget-set! gui duration-field 'align GUI_ALIGNCENTER)
(set! duration-range (num-only 1 600000 (glgui-widget-get gui duration-field 'label)))
(glgui-widget-set! gui duration-field 'aftercharcb (lambda (parent widget event x y) (duration-range parent widget)))
(glgui-label gui 195 40 40 30 "ms" ascii_18.fnt *foreground-color*)
```

Frequency is rather abstract. Let's also give the user the ability to select a
musical note. We can store the corresponding frequencies for A4-G4 in a table.

```scheme
;; Notes -> frequency (middle A-G [A4-G4])
;; http://pages.mtu.edu/~suits/notefreqs.html
(define notes (list->table '((0 . 440.00)    ; A
                             (1 . 493.88)    ; B
                             (2 . 261.63)    ; C
                             (3 . 293.66)    ; D
                             (4 . 329.63)    ; E
                             (5 . 349.23)    ; F
                             (6 . 292.00)))) ; G
```

We'll give the user a drop-down menu. Whenever a note is selected from the
drop-down menu, we'll look up the frequency in the table and set it using the
`set-frequency` helper function we created for the octave buttons.

```scheme
(glgui-label gui 410 40 60 30 "Note" ascii_18.fnt *foreground-color*)
(set! note (glgui-dropdownbox gui 470 40 50 30
  (map (lambda (str)
    (lambda (lg lw x y w h s) (if s (glgui:draw-box x y w h *foreground-color*))
      (glgui:draw-text-left (+ x 5) y (- w 10) h str ascii_18.fnt *text-color*)))
    (list "A" "B" "C" "D" "E" "F" "G"))
  *accent-color* *foreground-color* *accent-color*))
(glgui-widget-set! gui note 'scrollcolor *accent-color*)
(glgui-widget-set! gui note 'callback (lambda (parent widget event x y)
  (set-frequency (table-ref notes (glgui-widget-get parent widget 'current)))))
```

Now, let's make some noise. LambdaNative has a rtaudio module. We'll use that
to generate a tone with a sine wave. Edit the `MODULES` file in your
applications subdirectory and add rtaudio to the list. The Scheme API of the
rtaudio module consists of essentially just two functions: `rtaudio-start` and
`rtaudio-stop`. You must first register four real-time hooks (an initialization
hook, input hook, output hook, and close hook) in a chunk of C code embedded
within your Scheme code. I wish the rtaudio module had an API that allowed
implementing these hooks in pure Scheme. Thankfully the
[DemoRTAudio](https://github.com/part-cw/lambdanative/tree/master/apps/DemoRTAudio)
app included with LambdaNative implements a sine wave, and I was able to copy
and paste most of what I needed from there without spending a lot of time
trying to figure out how to write a sine wave in C myself.

```scheme
;; Register C-side real-time audio hooks
(c-declare  #<<end-of-c-declare

#include <math.h>

void rtaudio_register(void (*)(int), void (*)(float), void (*)(float*,float*), void (*)(void));

double f;
double srate=0;
float buffer;

void my_realtime_init(int samplerate) { srate=(double)samplerate; buffer=0; }
void my_realtime_input(float v) { }
void my_realtime_output(float *v1,float *v2) {
  static double t=0;
  buffer = 0.95*sin(2*M_PI*f*t);
  *v1=*v2=(float)buffer;
  t+=1/srate;
}
void my_realtime_close() { buffer=0; }

end-of-c-declare
)
(c-initialize "rtaudio_register(my_realtime_init,my_realtime_input,my_realtime_output,my_realtime_close);")
```

The [basic formula for a sine
wave](http://pld.cs.luc.edu/telecom/mnotes/digitized_sound.html) is A sin(2πft)
where *A* is amplitude, *f* is frequency, and *t* is time. We need a way to
pass the frequency from our slider in the Scheme to the output hook in the C.
Gambit scheme has a `c-lambda` special form that makes it possible to create a
Scheme function that is a representative of a C function or code sequence.

```scheme
(define rtaudio-frequency (c-lambda (double) void "f=___arg1;"))
```

This creates a Scheme function that sets the f variable in our C chunk. Now
let's create a Schem function that will set the frequency and start and stop
the real-time audio subsystem.

```scheme
;; Generate a tone using the rtaudio module
(define (generate-tone parent widget event x y)
  ; Make sure neither frequency or duration were left blank
  (if (= (string-length (glgui-widget-get parent frequency-field 'label)) 0) (set-frequency 1))
  (if (= (string-length (glgui-widget-get parent duration-field 'label)) 0) (glgui-widget-set! parent duration-field 'label "1"))
  (rtaudio-frequency (exact->inexact (string->number (glgui-widget-get parent frequency-field 'label))))
  (rtaudio-start 44100 0.5)
  (thread-sleep! (/ (string->number (glgui-widget-get parent duration-field 'label)) 1000))
  (rtaudio-stop))
```

When playing a note such as B4 (493.88 Hz) that has a decimal point, the type
passed from Scheme to C lines up with the C type `float`, but when passing an
integer (such as 440), it will cause an error. The `exact->inexact` conversion
forces Scheme to pass the value along as a `float`. Wire this up to the play
button, and you're ready to make some noise.

```scheme
(set! play-button (glgui-button-string gui 230 125 80 50 "Play" ascii_25.fnt generate-tone))
```

LambdaNative has a lot of rough edges, not least of which is the documentation
(or lack thereof). Looking at the source code for a widget seems to be the only
way to determine all the parameters available for that widget. If you're like
me, being able to write mobile apps in Lisp is a dream come true! LambdaNative
may not be the smoothest development experience right now, but I hope to
revisit it again in the future. It is being actively developed (and has the
backing of a university research team), so my hopes are high for the future of
LambdaNative.
