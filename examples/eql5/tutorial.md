Embedded Qt5 Lisp (EQL5) is a Qt5 binding for Embedded Common Lisp (ECL). EQL5
is a bit different than most bindings. Instead of executing your Lisp source
with `ecl` and importing the bindings, you compile a new interpreter (`eql5`)
that combines both ECL and Qt5 bindings. Instead of building yet another
calculator, let's build a GUI for generating a tone.

There are ports of EQL5 to Android and iOS, and a work-in-progress port to
Sailfish OS, so EQL5 can also be used to develop mobile apps. The "Embedded"
part of the name (of both ECL and EQL5) refers to the fact that it can be
embedded in existing C++ projects. For this tutorial, we'll be building a
standalone desktop app.

![Screenshot](../../screenshots/eql5.png?raw=true "Example screenshot")

## Compiling EQL5

EQL5 doesn't appear to be in the repos of any major Linux distro. It is
available from AUR on Arch, but since I use Debian, I compiled it from source.
Don't worry. It's not as scary as it sounds.

* Clone the EQL5 Git repo. EQL5 doesn't appear to have releases or tags, so I
guess you just clone `master` and hope that it's in a workable state.

```console
$ git clone https://gitlab.com/eql/EQL5.git
```

* Install dependencies. This was actually the hardest part. Not the installing.
That's just `apt install whatever` (or the package manager of your preferred
distro). There are hundreds of Qt packages on Debian, and it's not always clear
what Qt module corresponds to what Debian package (here's a
[list](https://askubuntu.com/a/577334) I found useful). If I got an unknown
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

```console
$ sudo apt install ecl libqt5svg5-dev qml-module-qtquick2 qtbase5-dev qtdeclarative5-dev qtmultimedia5-dev qttools5-dev qtwebengine5-dev qml-module-qtquick-controls2 qml-module-qtquick-layouts qml-module-qtquick-window2 qml
```

* Navigate into the `src` subdirectory of the cloned EQL5 Git repo, compile,
and install. The `-qt5` flag was necessary on Debian, because `qmake` is a
symlink to `qtchooser`. On other distros, this flag may not be required.

```console
$ cd EQL5/src
$ ecl -shell make
$ qmake -qt5 eql5.pro
$ make
$ sudo make install
```

* Now `eql5` should be available to run. Verify it with `eql5 -qgui`, which
will launch a GUI REPL. This also gives you access to some documentation on the
EQL5 functions. Go the the *Help* tab for a searchable list of functions.

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

The syntax of QML looks like a cross between JSON and CSS. Objects are
specified followed by a pair of braces. Properties of the object are specified
with key-value colon-separated pairs. You can create white text on a blue
background like this:

```qml
import QtQuick 2.0

Rectangle {
  width: 200
  height: 100
  color: "blue"

  Text {
    anchors.centerIn: parent
    color: "white"
    text: "Hello, world!"
  }
}
```

You can build custom GUI elements out of rectangles and other drawing
primitives, but it would be nice to have some pre-defined widgets to use. Let's
also import Qt Quick Controls and Qt Quick Layouts. You will need to have both
modules installed on your system. They are probably available in your distro's
package manager (see above for installing them on Debian).

```qml
import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.2
```

For maximum compatibility, I recommend specifying the minimum version of
modules. You might have Qt Quick Controls 2.15 on the machine your developing
on, but one of your users might have an earlier version. Only bump the version
if a later version has features you need to use. For example,
[`Layout.margins`](https://doc.qt.io/qt-5/qml-qtquick-layouts-layout.html#margins-attached-prop)
wasn't introduced until Qt Quick Layouts 1.2. We'll be setting margins in the
example we're building with this tutorial, so I imported version 1.2 of Qt
Quick Layouts.

We arrange items in our UI using `ColumnLayout` and `RowLayout` from Qt Quick
Layouts. `ColumnLayout` arranges its childrent vertically, and `RowLayout`
arranged its children horizontally. At the top, there will be a slider, so
we'll put a `Slider` within a `ColumnLayout`.

```qml
ColumnLayout {
  id: root

  Slider {
    id: frequencySlider
    from: 20
    value: 440
    to: 20000
    Layout.fillWidth: true
    Layout.margins: 25
  }
}
```

The range of frequencies audible by humans is typically between 20 Hz and 20
KHz (we lose the ability to hear some of those higher frequencies as we age).
The [musical note A above middle
C](https://en.wikipedia.org/wiki/A440_(pitch_standard)) is 440 Hz. Since A4
serves as a general tuning standard, it seems like a sensible default.
`Layout.fillWidth` makes the slider take up the full width of the window.

Under the slider, there will be a row (a great use for `RowLayout`) with two
buttons and a spin box (a widget for entering a number within a fixed range).

```qml
RowLayout {
  spacing: 25
  Layout.alignment: Qt.AlignHCenter
  Layout.margins: 25

  Button {
    text: "<"
  }

  RowLayout {
    SpinBox {
      id: frequencyField
      editable: true
      from: 20
      value: frequencySlider.value
      to: 20000
    }
    Label { text: "Hz" }
  }

  Button {
    text: ">"
  }
}
```

Under that is another row of controls: another spin box, a button, and a
drop-down menu to select notes from. Frequency is rather abstract. The
drop-down gives the user the ability to select a musical note.

```qml
RowLayout {
  spacing: 25
  Layout.margins: 25

  RowLayout {
    SpinBox {
      id: durationField
      editable:true
      from: 1
      value: 200
      to: 600000
    }
    Label { text: "ms" }
  }

  Button {
    text: "Play"
  }

  RowLayout {
    Label { text: "♪" }
    ComboBox {
      model: ["A", "B", "C", "D", "E", "F", "G"]
    }
  }
}
```

I started this project with zero knowledge of QML. I went from Googling "qml"
to the complete mockup above in about 30 minutes. I haven't written any of the
application logic (in fact, we haven't even touched Common Lisp yet), but you
can see how powerful a declarative syntax like QML is for defining GUIs. On big
teams, you could even have a designer write the QML while a backend programmer
wrote the logic. If you installed the QML viewer above (the qml package on
Debian), you can preview your GUI with:

```console
qml bleep.qml
```

![Screenshot](../../screenshots/eql5-mockup.png?raw=true "QML preview")

## Writing the Logic in Common Lisp

Copy `qml-lisp.lisp` from the EQL5 example directory
(`EQL5/examples/M-modules/quick/qml-lisp/qml-lisp.lisp`) to your working
directory. This enables QML to call Lisp functions and vice versa. I'm not sure
why this isn't built into EQL5, but it's easy enough to copy it from the Qt
Quick examples that come with EQL5. Also, create a `bleep.lisp` for the program
logic.

```lisp
(qrequire :quick) ; Have EQL5 use Qt Quick module from Qt
(require :qml-lisp "qml-lisp") ; Load qml-lisp.lisp package copied from example
(use-package :qml) ; Import all external symbols from above package
```

The `qrequire` function is part of the EQL5 bindings. It loads a specified Qt
module. In our case, we want to load the Qt Quick module. Then we load the Lisp
package we copied from the EQL5 examples that enables communication between
Lisp and QML. We'll use these two imports to run the QML UI we created above.

```lisp
(defun run ()
  ; QQuickView provides a window for displaying a Qt Quick user interface:
  ; https://doc.qt.io/qt-5/qquickview.html
  (setf qml:*quick-view* (qnew "QQuickView"))
  (x:do-with qml:*quick-view*
    (|setSource| (|fromLocalFile.QUrl| "bleep.qml"))
    (|setTitle| "Bleep")
    (|show|)))

(run)
```

The `*quick-view*` variable is supplied by the `qml-lisp.lisp` file we copied
over (hence the `qml` namespace). It can be either a QQuickView or QQuickWidget
Qt object. We use `qnew` from EQL5 to create a
[QQuickView](https://doc.qt.io/qt-5/qquickview.html) object. The `qnew`
function creates a Qt object of a given class.

The `x` namespace includes utility functions and macros that come with EQL5.
Since this namespace is built into EQL5, I would have thought the `qml`
namespace could have been too. The `x` namespace does not appear to be
documented anywhere. I copied the usage of `do-with` from the Qt Quick examples
that come with EQL5. It basically let's us chain together a bunch of Qt methods
on the `*quick-view*` object.

EQL5 comes with `qfun` for calling Qt methods on objects. For example, `(qfun
qml:*quick-view* "show")` would call the `show` method on the
`qml:*quick-view*` object. EQL5 also comes with a shorthand for this: `(|show|
qml:*quick-view*)`. We can use this shorthand to set the source of the
QQuickView to the QML file we created above, set a title for the window, and
finally call the `show` method to display the window. Execute this with `eql5`,
and you should get a window that looks just like the window you previewed above
with `qml`.

```console
$ eql5 bleep.lisp
```

You can tell that the default style has been designed to be touch friendly. The
handle on the slider is extra big so that it can be dragged with a finger. All
the buttons have plenty of padding to make them easy targets. Qt Quick also
comes with a platform-agnostic style called Fusion that offers a
desktop-oriented look and feel.

There are [four
ways](https://doc.qt.io/QT-5/qtquickcontrols2-styles.html#using-styles-in-qt-quick-controls)
to set the style. The first is using the `QQuickStyle` class, but this class is
not wrapped by EQL5. The second is to pass a `-style` command line argument.
Asking our user to include a command line argument to get the desired styling
seems a bit much. The next option is to set a `QT_QUICK_CONTROLS_STYLE`
environment variable. ECL has
[`ext:setenv`](https://common-lisp.net/project/ecl/static/manual/Operating-System-Interface.html)
for setting environment variables. We'll set the environment variable before
defining `run` and then execute our code again:

```lisp
; Use the desktop-oriented style Fusion instead of the default
; https://doc.qt.io/QT-5/qtquickcontrols2-styles.html
; EQL5 doesn't wrap the QQuickStyle class so using environment variable
(ext:setenv "QT_QUICK_CONTROLS_STYLE" "fusion")
```

![Screenshot](../../screenshots/eql5-fusion.png?raw=true "Fusion style")

The scale of 20 to 20,000 is so large that 440 doesn't appear to move the
slider at all. Ideally, 440 would fall about the middle of the slider. To
achieve this, let's use a logarithmic scale.

I found a [Stack Overflow
answer](https://stackoverflow.com/questions/846221/logarithmic-slider/846249#846249)
on how to map a slider to a logarithmic scale. The code given in the answer is
JavaScript. We can embed JavaScript directly into our QML, so we could use the
JavaScript example. This being a Lisp tutorial, we are going to port it to
Common Lisp.

```lisp
; Scale used by slider
(defparameter *min-position* 0)
(defparameter *max-position* 2000)
; Range of frequencies
(defparameter *min-frequency* 20)
(defparameter *max-frequency* 20000)

; Logarithmic scale for frequency (so middle A [440] falls about in the middle)
; Adapted from https://stackoverflow.com/questions/846221/logarithmic-slider

(defvar min-freq (log *min-frequency*))
(defvar max-freq (log *max-frequency*))
(defvar frequency-scale (/ (- max-freq min-freq) (- *max-position* *min-position*)))
; Convert slider position to frequency
(defun position->frequency (position)
  (round (exp (+ min-freq (* frequency-scale (- position *min-position*))))))
; Convert frequency to slider position
(defun frequency->position (freq)
  (round (/ (- (log freq) min-freq) (+ frequency-scale *min-position*))))
```

I added some global parameters to the top of the script. The variable name
`*min-position*` is just a Lisp naming convention for global parameters. I came
up with the range of 0-2,000 by trial and error. It seemed to strike the best
balance between each step of the slider making a noticeable change to the
frequency while still allowing the user to narrow in on a specific frequency
with just the slider. Then we create two functions: one that takes the position
on the slider and returns the frequency (`position->frequency`) and another
that takes a frequency and returns the position on the slider
(`frequency-position`).

It would be useful if these global parameters were available both to Lisp and
QML. Qt has [a way to embed C++ Objects into
QML](https://doc.qt.io/qt-5/qtqml-cppintegration-contextproperties.html) by
setting a context property on the root context. We could pass the string "Bugs
Bunny" from C++ to QML this way:

```cpp
// C++
QString fictionalRabbit = "Bugs Bunny";
view.rootContext()->setContextProperty("fictionalRabbit", fictionalRabbit);
```

```qml
// QML
Text { text: fictionalRabbit }
```

Using EQL5 and Lisp, it would look like this:

```lisp
(|setContextProperty| (|rootContext| qml:*quick-view*)
  "fictionalRabbit" (qvariant-from-value "Bugs Bunny" "QString"))
```

The order is reversed in Lisp compared to C++. Whereas in C++ you have
`view.rootContext()`, in Lisp you do `(|rootContext| qml:*quick-view*)`. The
method to call comes first. Then the object to call that method on followed by
any additional arguments. The value passed must be a C++ object. EQL5 comes
with `qvariant-from-value` that makes it easy to construct a `QVariant` of a
specified type. That's more than I want to type for every variable I want to
export from Lisp into QML, so I whipped up a little helper function:

```lisp
; Helper function to make Lisp data available in QML
; Allows writing (set-context-property variable-in-lisp "variableInLisp" "QString")
; instead of (|setContextProperty| (|rootContext| qml:*quick-view*)
;   "variableInLisp" (qvariant-from-value variable-in-lisp "QString"))
(defun set-context-property (lisp-var qml-name type-name)
  (let ((root-context (|rootContext| qml:*quick-view*))
        (objectified (qvariant-from-value lisp-var type-name)))
    (|setContextProperty| root-context qml-name objectified)))
```

Then I added this to my `run` function:

```lisp
; Make data available in QML
(set-context-property *min-position* "minPosition" "int")
(set-context-property *max-position* "maxPosition" "int")
(set-context-property *min-frequency* "minFrequency" "int")
(set-context-property *max-frequency* "maxFrequency" "int")
```

We now need to modify our QML to use these Lisp functions and variables. The
first thing we need to do is add another `import` statement at the top.

```qml
import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.2
import EQL5 1.0
```

This imports everything we need to call Lisp functions from QML.

If you played around with the QML mockup above, you may have noticed moving the
slider automatically updated the spin box underneath it. This was done with a
property binding. The value of the spin box was set to `frequencySlider.value`.
Whenever the slider value changes, the spin box value automatically updates.
This was fine for a quick mockup, but we'd like for it to work both ways. When
you change the value of the spin box, the slider should also automatically
update. Unfortunately, QML doesn't support [bi-directional property
bindings](http://imaginativethinking.ca/bi-directional-data-binding-qt-quick/).
What we'll do is create a property that we'll bind both the slider and spin box
to. Then we'll create handlers in both to update this property.

```qml
ColumnLayout {
  id: root

  property real frequency: 440

  Slider {
    id: frequencySlider
    from: minPosition
    value: Lisp.call("frequency->position", root.frequency)
    to: maxPosition
    onValueChanged: root.frequency = Lisp.call("position->frequency", value)
    Layout.fillWidth: true
    Layout.margins: 25
  }

...

    RowLayout {
      SpinBox {
        id: frequencyField
        editable: true
        from: minFrequency
        value: root.frequency
        to: maxFrequency
      }
      Label { text: "Hz" }
    }
...
}
```

The range for the slider and spin box is now defined using the context
properties set in Lisp. To call Lisp functions, use `Lisp.call()`. It's not
pretty (the function name must be quoted and the list comma separated), but it
gets the job done. If you run the code with EQL5 from a terminal, you can move
the slider, and the spin box will update accordingly, but you'll get a whole
bunch of these warnings in your terminal:

```
QML Slider: Binding loop detected for property "value"
```

Whenever you move the slider, `onValueChanged` updates the `frequency`
property. When `frequency` changes this triggers another update of the slider,
which in turn, updates `frequency` again, creating a loop. Qt Quick Controls
2.2 and later have `onMoved` in addition to `onValueChanged`. If you swap
`onValueChanged` out for `onMoved`, it breaks the loop. With `onMoved`, the
slider only updates the `frequency` property when interactively moved. When a
change to `frequency` triggers a value change of the slider, it will not be
propogated back to `frequency` avoiding the binding loop.

The other problem with our quick QML mockup upon closer inspection is that the
QML spin box only works on integers. We want more precision than that to
represent notes. For example, middle C is 261.63. Qt Widgets has
[`QDoubleSpinBox`](https://doc.qt.io/qt-5/qdoublespinbox.html), but Qt Quick
doesn't have an [equivalent](https://bugreports.qt.io/browse/QTBUG-67349).

There is an
[example](https://doc.qt.io/qt-5/qml-qtquick-controls2-spinbox.html#custom-values)
in the Qt documentation of the QML SpinBox on how it can be customized to
accept floating point numbers. The example as-is truncates digits after 2
decimal places, thus 500.157 would become 500.15. This is probably contrary to
what users expect, so I added `Math.round` to `value` and `valueFromText`.

```qml
SpinBox {
  id: frequencyField
  editable: true
  from: minFrequency * 100
  value: Math.round(root.frequency * 100)
  to: maxFrequency * 100
  stepSize: 100
  onValueChanged: root.frequency = value / 100

  property int decimals: 2
  property real realValue: value / 100

  validator: DoubleValidator {
    bottom: Math.min(frequencyField.from, frequencyField.to)
    top: Math.max(frequencyField.from, frequencyField.to)
  }

  textFromValue: function(value, locale) {
    return Number(value / 100).toLocaleString(locale, 'f', decimals)
  }

  valueFromText: function(text, locale) {
    return Math.round(Number.fromLocaleString(locale, text) * 100)
  }
}
```

On either side of the spin box underneath the slider are buttons to
increase/decrease the frequency by one octave. This could be done with a little
JavaScript right in the QML, but since this is a Lisp tutorial, we're going to
do it with Common Lisp. You can access QML properties from Lisp, but it needs
`objectName` to be set. To have uniform access to QML items from both QML and
Lisp, it is convenient to set both `id` and `objectName` to the same name.

```qml
ColumnLayout {
  id: root
  objectName: "root"

  property real frequency: 440

  ...
}
```

Note the double quotes around the `objectName`. It can't be a bare keyword. It
must be a string. We can then access properties of this item from Lisp with
`q<` and `q>` (both from `qml-lisp.lisp` that we copied over.) They are
shorthands for `qml-get` and `qml-set`, respectively.

```lisp
; Buttons increase and decrease frequency by one octave
(defun adjust-octave (modifier)
  (let* ((freq (q< |frequency| "root"))
         (new-freq (* freq modifier)))
    (unless (or (< new-freq *min-frequency*) (> new-freq *max-frequency*))
      (q> |frequency| "root" new-freq))))
(defun decrease-octave () (adjust-octave 0.5))
(defun increase-octave () (adjust-octave 2))
```

Wire the QML buttons up to these functions. An
[octave](https://en.wikipedia.org/wiki/Octave) is "the interval between one
musical pitch and another with double its frequency."

```qml
Button {
  text: "<"
  onClicked: Lisp.call("decrease-octave")
}

...

Button {
  text: ">"
  onClicked: Lisp.call("increase-octave")
}
```

We gave the user a drop-down menu (combo box) in our QML mockup. Whenever a
note is selected from the drop-down menu, we'll look up the frequency in a
model and update the `frequency` property.

```qml
ComboBox {
  textRole: "note"
  // Notes -> frequency (middle A-G [A4-G4])
  // http://pages.mtu.edu/~suits/notefreqs.html
  model: [
    { note: "A", freq: 440.00 },
    { note: "B", freq: 493.88 },
    { note: "C", freq: 261.63 },
    { note: "D", freq: 293.66 },
    { note: "E", freq: 329.63 },
    { note: "F", freq: 349.23 },
    { note: "G", freq: 392.00 }
  ]
  onActivated: root.frequency = model[index]["freq"]
}
```

I also had to add `(si::trap-fpe t nil)` to the top of my Lisp file. If not, I
got a `Condition of type: DIVISION-BY-ZERO` error whenever trying to expand the
combo box. I'm not sure exactly why. I just copied it from one of the Qt Quick
examples bundled with EQL5. There were no comments or documentation explaining
its purpose.

Now, let's make some noise.

```lisp
(ql:quickload "cl-portaudio") ; Use Quicklisp to load CL-PortAudio

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
            (make-array frames-per-buffer :initial-contents
              (loop for j from (+ (* frames-per-buffer i) 1) to (* frames-per-buffer (+ i 1)) collect
                (let ((time (/ j sample-rate)))
                  (* amplitude (sin (* 2 pi frequency time))))))))))))
```

We'll use [Common Lisp bindings to
PortAudio](https://github.com/filonenko-mikhail/cl-portaudio) to generate the
tone. This can be installed with [Quicklisp](https://www.quicklisp.org/). If
you don't already have Quicklisp installed, it's painless. See the Quicklisp
website for more details, but here's an example of installing Quicklisp on
Debian and configuring ECL. The steps should be the same for any Linux distro
and macOS.

```console
$ curl curl -O https://beta.quicklisp.org/quicklisp.lisp
$ ecl -load quicklisp.lisp
> (quicklisp-quickstart:install)
> (ql:add-to-init-file)
```

The first time you run `bleep.lisp`, it will take awhile as Quicklisp downloads
CL-PortAudio (by default it will be downloaded to `~/quicklisp`). The "proper"
way to include this dependency would be to use
[ASDF](https://common-lisp.net/project/asdf/) and create a `.asd` file for the
project. EQL5 includes [an
example](https://gitlab.com/eql/EQL5/-/tree/master/my_app) of how to package an
EQL5 app with ASDF. Since this is a quick tutorial, I'll stick with
`ql:quickload`.

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

Wire this up to the play button in the QML, and you're ready to make some
noise.

```qml
Button {
  text: "Play"
  onClicked: Lisp.call("generate-tone", frequency, durationField.value)
}
```
