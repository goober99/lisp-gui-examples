LambdaNative is a cross-platform framework for developing desktop and mobile
apps using Scheme (built atop Gambit). Resources on LambdaNative seem to be
extremely scarce. I couldn't find a single step-by-step tutorial for using
LambdaNative, so hopefully my small contribution will benefit others trying to
get started with LambdaNative.

LambdaNative already includes a calculator demo, and frankly, I find making a
millionth calculator example to be a little dull. Instead I'll be building a
GUI for generating a tone. We're only going to be building a desktop GUI, so we
aren't going to be tapping into the full power of LambdaNative. Where it seems
LambdaNative would really be useful is enabling you to write cross-platform
mobile apps with Scheme! Unfortunately, that is outside the scope of this
tutorial. Maybe in the future I'll revisit LambdaNative and use it to create a
mobile app.

![Screenshot](../../screenshots/lambdanative.png?raw=true "Example screenshot")

## Installing LambdaNative

The LambdaNative wiki gives a [list of
dependencies](https://github.com/part-cw/lambdanative/wiki/Getting-Started#required-tools-and-libraries)
that need to be installed with your distro's package manager before installing
LambdaNative. They give an `apt` command you can copy and paste to your
terminal to install all the needed dependencies on Ubuntu.

* Download the most recent [release](https://github.com/part-cw/lambdanative/releases).

* Unzip the release to a system-wide location such as `/opt` or `/usr/local`.

```bash
sudo unzip lambdanative-*.zip -d /opt
```

* Rename unzipped directory.

```bash
cd /opt
sudo mv lambdanative* lambdanative
```

* Create the files `SETUP` and `PROFILE`. If you were developing a mobile app,
you would need to configure these files for the respective SDKs. Since that is
outside the scope of this tutorial, that is left as an exercise for the reader.

```bash
cd lambdanative
sudo cp SETUP.template SETUP
sudo cp PROFILE.template PROFILE
```

* Edit `scripts/lambdanative` and populate the `LAMBDANATIVE` variable with
`/opt/lambdanative`.

```bash
LAMBDANATIVE=/opt/lambdanative
```

* Place the LambdaNative initialization script in the system path.

```bash
sudo ln -s /opt/lambdanative/scripts/lambdanative /usr/bin/lambdanative
```

* Create and initialize a LambdaNative build directory.

```bash
mkdir ~/lambdanative
cd ~/lambdanative
lambdanative init
```

## Creating a New GUI App

Your freshly initiated build directory will look like this:

```
apps/
modules/
configure
Makefile
```

Your app will go in its own subdirectory in `apps`. To create a new app:

```bash
lambdanative create <appname> <apptype>
```

The options for `<apptype>` are `console`, `gui`, and `eventloop`. I created a
new GUI app called bleep:

```bash
lambdanative create bleep gui
```

This will create a directory in `apps` called `bleep` with several files in it.

## Compiling the App

LambdaNative utilizes the GNU Build System.

```bash
./configure bleep
make
make install
```

By default, the build will target the local host. The first time you do this
will take awhile, because it is downloading and compiling prerequisites. These
prerequisites are cached to speed up subsequent compiles. If you get any errors
during the initial build, you probably missed installing a dependency. Install
it with your distro's package manager and then try compiling again.

You can also configure in debug mode. You will want to clean the cache with
`make scrub` so everything is rebuilt. It will take awhile since everything is
being rebuilt.

```bash
./configure bleep debug
make scrub
make
make install
```

In my experience, debug mode didn't help much. Runtime errors are logged to
`~/Desktop/log/*.txt`.

```
[SYSTEM] 2019-03-16 00:59:21: Application bleep built 2019-03-16 00:59:14
[SYSTEM] 2019-03-16 00:59:21: Git hash
[ERROR] 2019-03-16 00:59:22: primordial: (assoc 49 #f): (Argument 2) LIST expected
[ERROR] 2019-03-16 00:59:22: HALT
```

Not very helpful, is it? So I enabled debug mode.

```
[SYSTEM] 2019-03-16 01:16:44: Application bleep built 2019-03-16 01:16:34
[SYSTEM] 2019-03-16 01:16:44: Git hash
[ERROR] 2019-03-16 01:16:44: primordial: (assoc 49 #f): (Argument 2) LIST expected
[ERROR] 2019-03-16 01:16:44: trace: /opt/lambdanative/modules/ln_glgui/primitives.scm line=230 col=21
[ERROR] 2019-03-16 01:16:44: trace: /opt/lambdanative/modules/ln_glgui/primitives.scm line=273 col=21
[ERROR] 2019-03-16 01:16:44: trace: /opt/lambdanative/modules/ln_glgui/slider.scm line=80 col=8
[ERROR] 2019-03-16 01:16:44: trace: /opt/lambdanative/modules/ln_glgui/glgui.scm line=151 col=36
[ERROR] 2019-03-16 01:16:44: trace: /opt/lambdanative/modules/ln_glgui/glgui.scm line=145 col=11
[ERROR] 2019-03-16 01:16:44: trace: /opt/lambdanative/modules/ln_glgui/glgui.scm line=183 col=3
[ERROR] 2019-03-16 01:16:44: trace: /opt/lambdanative/modules/eventloop/eventloop.scm line=151 col=9
[ERROR] 2019-03-16 01:16:44: HALT
```

It's definitely longer but still not very helpful. It actually includes line
numbers, which appears promising, but they are all line numbers in LambdaNative
modules. It doesn't actually trace the error all the way to the line in my app
causing the error. I spent a lot more time reading the source of the
LambdaNative modules than I would have liked.

And that's when the log file even contained an error. Sometimes the app failed
with a segmentation fault and there was no error in the log at all. I often
peppered my source with `(log-status "reached here")` and `tail -f` the log
file to debug and isolate errors.

Errors caught at compile time, on the other hand, were much nicer. If an error
occurred while compiling, the error message included the line number from my
app.

The development workflow is more akin to traditional compiled languages like C.
I missed the quick feedback I'm used to while developing Scheme on a REPL.
After the initial compile, subsequent compiles are much quicker.

```bash
$ time make

real  0m3.877s
user  0m2.652s
sys   0m0.605s
```

Four seconds can seem like a long time when you are making a series of small
changes or chasing down a bug. There is a
[module](https://github.com/part-cw/lambdanative/wiki/Using-Emacs) for REPL
editing with Emacs. Since I don't use Emacs, I didn't give it a spin, but if I
was developing a larger app, I might give it a try.

The `install` step will move the executable to `~/Desktop/bleep/bleep` and
launch it. This will launch a rectangular window.

![Screenshot](../../screenshots/lambdanative-rectangle.png?raw=true "Rectangular window")

This window looks awful lonely. Let's add some widgets!

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
(define *min-frequency* 1)
(define *max-frequency* 19999)
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
(set! frequency-field (glgui-inputlabel gui 210 230 120 30 "440 Hz" ascii_18.fnt *text-color* *foreground-color*))
(glgui-widget-set! gui frequency-field 'align GUI_ALIGNCENTER)

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
  (glgui-widget-set! gui frequency-field 'label (string-append (number->string
    (position->frequency (glgui-widget-get gui slider 'value))) " Hz")))
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

Before we can do anything with the text display of the frequency, we need a
helper function that will strip the "Hz" string from the value, so we're left
with a number we can do math operations with.

```scheme
;; Chop off units (Hz) from value
(define (chop-units text)
  (let* ((text-length (string-length text))
         ; Prevent negative position in string if text-length shorter than units
         (text-pos (if (< (- text-length 3) 0) 0 (- text-length 3)))
         (text-units (substring text text-pos text-length)))
    (if (equal? text-units " Hz") (substring text 0 text-pos) text)))
```

I replaced the anonymous lambdas in the octave button declarations with
callback functions called `decrease-octave` and `increase-octave`. An
[octave](https://en.wikipedia.org/wiki/Octave) is "the interval between one
musical pitch and another with double its frequency."

```scheme
;; Set frequency slider and display
(define (set-frequency freq)
  (glgui-widget-set! gui slider 'value (frequency->position freq))
  (glgui-widget-set! gui frequency-field 'label (string-append (number->string freq) " Hz")))
;; Buttons increase and decrease frequency by one octave
(define (adjust-octave modifier)
  (let ((new-freq (* (string->number (chop-units (glgui-widget-get gui frequency-field 'label))) modifier)))
    (if (and (>= new-freq *min-frequency*) (<= new-freq *max-frequency*)) (set-frequency new-freq))))
(define (decrease-octave parent widget event x y) (adjust-octave 0.5))
(define (increase-octave parent widget event x y) (adjust-octave 2))
```

The `glgui-inputlabel` has two callback parameters available: `'onfocuscb` and
`'aftercharcb`. It would be nice to strip the "Hz" from the value on focus, so
the user is left with just a number to edit. I created a new function similar
to the `chop-units` helper function I created above, this time with an
exclamation mark since this function has side effects (changing the contents of
the text field).

```scheme
(define (chop-units! parent widget event x y)
  (glgui-widget-set! parent widget 'label (chop-units (glgui-widget-get parent widget 'label))))
```

The `'aftercharcb` callback is called after each character is typed or deleted.
We can use this to update the slider as a user enters a frequency. What if a
user (and you know they will) enters a number higher than 20,000 or a letter?
We need a function that will only allow numbers within a given range.

```scheme
;; Only allow numbers within range of min-value and max-value
(define (num-only min-value max-value old-value)
  (lambda (parent widget)
    (let* ((current-value (chop-units (glgui-widget-get parent widget 'label)))
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

Now we can wire the `glgui-inputlabel` callbacks up to these functions.

```scheme
(glgui-widget-set! gui frequency-field 'onfocuscb chop-units!)
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

We can use the `chop-units!` helper function and `num-only` closure again to
create a field to specify the duration of the beep in milliseconds:

```scheme
;; General Controls
(glgui-label gui 20 40 80 30 "Duration" ascii_18.fnt *foreground-color*)
(set! duration-field (glgui-inputlabel gui 110 40 120 30 "200 ms" ascii_18.fnt *text-color* *foreground-color*))
(glgui-widget-set! gui duration-field 'align GUI_ALIGNCENTER)
(glgui-widget-set! gui duration-field 'onfocuscb chop-units!)
(set! duration-range (num-only 1 600000 (glgui-widget-get gui duration-field 'label)))
(glgui-widget-set! gui duration-field 'aftercharcb (lambda (parent widget event x y) (frequency-range parent widget)))
```

And a little change to `chop-units`, so it will chop both "Hz" and "ms".

```scheme
;; Chop off units (Hz and ms) from value
(define (chop-units text)
  (let* ((text-length (string-length text))
         ; Prevent negative position in string if text-length shorter than units
         (text-pos (if (< (- text-length 3) 0) 0 (- text-length 3)))
         (text-units (substring text text-pos text-length)))
    (if (or (equal? text-units " Hz") (equal? text-units " ms")) (substring text 0 text-pos) text)))
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
  (if (= (string-length (glgui-widget-get parent duration-field 'label)) 0) (glgui-widget-set! parent duration-field 'label "1 ms"))
  (rtaudio-frequency (exact->inexact (string->number (chop-units (glgui-widget-get parent frequency-field 'label)))))
  (rtaudio-start 44100 0.5)
  (thread-sleep! (/ (string->number (chop-units (glgui-widget-get parent duration-field 'label))) 1000))
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
