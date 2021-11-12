# Survey of the State of GUI Programming in Lisp

The state of cross-platform GUI programming is
[dire](https://blog.johnnovak.net/2016/05/29/cross-platform-gui-trainwreck-2016-edition/).
It's no surprise. It's a hard problem to solve. If you want to use native
controls, you have to deal with the incompatible APIs of each platform. If you
draw your own controls, there's a hundred small things that are difficult to
get right just for a simple text field (copy/paste, undo/redo, RTL, etc.).

If any language (or family of languages) has the facilities to handle the
complexities of cross-platform GUIs, surely it would by my beloved Lisp. I
usually avoid GUI programming. Often the terminal is all I need, but sometimes
it would be nice to have a GUI. If I want to create a cross-platform GUI, and I
want to write it in Lisp, what are my options?

I decided to set out to create the same example GUI in each production-quality,
actively maintained Lisp GUI library to compare them. Production-quality and
actively maintained are each somewhat subjective, and since I'm the one writing
this, I'm the final arbiter. Production-quality means the library is not
experimental, a WIP, or someone's half-finished bindings to a GUI toolkit.
Actively maintained doesn't necessarily mean a release within x months.
Sometimes a stable library can go a couple years between releases. I'll also
look to see if the maintainer seems to be responsive to issues and pull
requests. If you think I missed a library that I should try out, let me know.

I didn't want to create the umpteenth calculator or todo example, so I built a
simple GUI for generating a tone. I also wrote tutorials for creating the
example with each toolkit. The tutorials have been posted to my
[blog](https://hashnode.com/series/survey-of-the-state-of-gui-programming-in-lisp-ckjrsx5ze0n0h9ds1grge79pf)
as well as being available in Markdown in the [GitHub
repo](https://github.com/goober99/lisp-gui-examples)) alongside the code of the
examples. The example is designed to be trivial enough to quickly implement
(since I'm implementing it multiple times) while also using enough controls as
to be able to make a reasonable assessment of each GUI library.

## Recommended

### Racket

**Links:** [Tutorial](https://blog.matthewdmiller.net/learn-racket-by-example-gui-programming) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/racket)<br>
**Lisp Dialect:** Racket / **GUI Toolkit:** Native (GTK on Linux)

<img align="right" width="320" height="205" src="https://raw.githubusercontent.com/goober99/lisp-gui-examples/master/screenshots/survey/racket-activitylog2.png" />

"I want to learn Lisp, but which one should I learn?" is a question that pops
up from time to time in places like Reddit. My answer is, "Just pick one. Once
you learn the basics, then you can compare dialects." If pressed further that
that's not a real answer, I'll recommend Racket because of Racket's
batteries-included philosophy. You can't get much more batteries included than
a built-in GUI library. It's even that rare species of cross-platform GUI
library that uses [native
controls](https://blog.racket-lang.org/2010/12/rebuilding-rackets-graphics-layer.html)
(GTK on Linux). While not as extensive as the widgets available if you were
using the native toolkits directly, Racket has all the [basic
widgets](https://docs.racket-lang.org/gui/Widget_Gallery.html) plus a [graph
plotting library](https://docs.racket-lang.org/plot/) and a [drawing
toolkit](https://docs.racket-lang.org/draw/index.html). Take a look at the
brilliant Alex Harsányi's
[ActivityLog2](https://github.com/alex-hhh/ActivityLog2), an app to analyze
data from various fitness activities, for an example of a complex GUI written
in Racket. If you stumbled upon this article looking for a cross-platform GUI
library for another language, it might be worth it to switch to Racket. If Lisp
is the hidden gem of programming languages, Racket just may be the hidden gem
of cross-platform GUI toolkits. No, really. You can stop reading here. Unless
you have specific requirements, Racket is the best cross-platform GUI toolkit
for Lisp (or any language).

Many of the other libraries in this survey are good libraries but have a [bus
factor](https://en.wikipedia.org/wiki/Bus_factor) of 1. Since the GUI library
is part of the Racket project (the DrRacket IDE that is bundled with Racket is
also built using the GUI library), there is no danger of it becoming
unmaintained. Like all of Racket, the documentation for the GUI library is
superb. Since the library is written in Racket, there is no C++ or Java
documentation that you must mentally translate the examples from.

There's even a WYSIWYG GUI designer for Racket ([MrEd
Designer](https://github.com/Metaxal/MrEd-Designer)) if you're so inclined. The
one thing on my wishlist that Racket lacks is a declarative syntax for
declaring GUIs, but Racket being Racket, it wouldn't be too difficult to build
a declarative DSL.

### Cljfx

Links: [Tutorial](https://blog.matthewdmiller.net/learn-clojure-by-example-javafx-gui-with-cljfx) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/cljfx)<br>
**Lisp Dialect:** Clojure / **GUI Toolkit:** JavaFX

<img align="right" width="350" height="242" src="https://raw.githubusercontent.com/goober99/lisp-gui-examples/master/screenshots/survey/cljfx-defold-editor.png" />

As effusive as I was about Racket, you might wonder why I have included a
second library under "Recommended." Clojure is arguably the most popular modern
Lisp dialect. It's [used](https://clojure.org/community/success_stories) by
companies like Walmart. With Clojure's Java interop, you have access to the
entire JVM ecosystem. Even with Racket's batteries-included philosophy and 3rd
party packages available with `raco`, the number of libraries available for
Racket pales in comparison to the number of Java libraries there are. There may
be already-existing Java libraries that integrate with APIs or provide features
that you need in your app.

Cljfx is a Clojure wrapper of JavaFX. JavaFX doesn't use native controls,
instead drawing its own controls. Swing apps have a dated look that I find
unpleasant, but I actually like the look of JavaFX. You can completely
customize the look and feel with CSS. If you want a custom look and feel for
your app instead of native controls, Cljfx would be a better choice than
Racket.

The [editor](https://github.com/defold/defold/tree/dev/editor) for the
[Defold](https://defold.com/) game engine is written in Clojure using Cljfx.
The developer of Cljfx is
[employed](https://www.youtube.com/watch?v=xcMNTKFmEgI) by King (the makers of
Candy Crush) to work on the Defold editor. The README reads like an extended
tutorial and there are lots of included examples, but other than that, there's
not really any documentation. While the README is informative, some
reference-style documentation would be nice. JavaFX is [well
documented](https://openjfx.io/) and has a large number of controls available,
but all the examples given are in Java, of course.

Cljfx is a declarative wrapper of JavaFX. I really like being able to build
GUIs declaratively. Instead of specifying the UI with a markup language like
other declarative toolkits (such as QML), Cljfx uses Clojure maps. This enables
some powerful features such as a composable UI, but maps are not as
designer-friendly as a markup language when coordinating with a designer.

One drawback with Cljfx and Clojure is startup time. On my desktop, it takes
nearly 5 seconds from when I launch the little example app I created until the
GUI appears on the screen. This is fine for big apps that you'll leave open and
spend a lot of time in after starting it (like a game editor). It might be
unacceptable for small apps that you open and close whenever you need them such
as a calculator or note taking app.

## Other Options (listed alphabetically)

Generally, choosing one of the above two recommended libraries is going to
result in the smoothest development experience, but they aren't the only two
ways to build a GUI with Lisp. Maybe you have special requirements or really
want to use a Lisp other than Racket or Clojure.

### EQL5

Links: [Tutorial](https://blog.matthewdmiller.net/learn-common-lisp-by-example-qt-gui-with-eql5) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/eql5)<br>
**Lisp Dialect:** Common Lisp / **Common Lisp Implementation:** ECL / **GUI Toolkit:** Qt

<img align="right" width="396" height="240" src="https://raw.githubusercontent.com/goober99/lisp-gui-examples/master/screenshots/survey/eql5-gym.png" />

Qt is a widely-used cross-platform GUI toolkit, but most of my reservations
about this library actually stem from Qt itself and not from the bindings to
Common Lisp. Qt has a split personality: Qt Widgets and Qt Quick. EQL5 supports
both. They primarily differ in how you define your UI. With Qt Widgets, you
declare it procedurally. With Qt Quick, you use a declarative language called
QML to define your UI. The underlying implementation of the widgets also
differ.

Qt Widgets has been around longer and has a broader range of available widgets.
As an example, Qt Widgets has both a spin box and a double spin box that
supports floating point numbers while Qt Quick only has the spin box which
supports integers. I used Qt Quick when building my example, and it required a
hack to get floating point numbers in the spin box since Qt Quick has no double
spin box.

Qt Widgets are more desktop-oriented without touch components and not easily
adapted to touch. Qt Quick, on the other hand, has been developed from the
beginning to work well on touch screens (tablets, smartphones, etc.). Qt Quick
is sometimes criticized for having a foreign look on the desktop (neither Qt
Quick or Qt Widgets use native widgets, but Qt Widgets look more native on the
desktop). Qt Quick can be styled and comes with a few built-in styles to choose
from including Fusion that offers a desktop-oriented look and feel. Creating
your own style is no easy feat though. To create a new style, you have to
provide alternate implementations of each widget with your styling using QML.
If you need a custom look and feel for your app, you'd be better off with Cljfx
since the look and feel of JavaFX can be customized much more easily with CSS.

Qt is essentially maintaining two sets of widgets. It would make more sense to
me if QML was just an alternative way of specifying which widgets to use
instead of a completely new set of widgets, but I'm not a Qt maintainer or even
a C++ developer. I guess creating new widgets was easier than adapting the old
widgets to be touch friendly. It's my understanding that the initial plan was
that Qt Quick would eventually replace Qt Widgets, but then Qt reversed that
decision, and now they are stuck maintaining two sets of widgets. I find
declarative ways to build GUIs a more natural fit for GUIs, so I consider it a
shame Qt reversed their initial plan to eventually replace Qt Widgets with Qt
Quick.

The integration between Common Lisp and QML is not the smoothest. For your Lisp
code to be able to interact with QML, it requires a Lisp source file with the
functions to interact with QML. This file can be copied from the QML examples
that come with EQL5, but I'm not sure why these functions aren't just built
into EQL5. To call Lisp functions from QML, you use `Lisp.call()`, which is
built into EQL5. It's not pretty (the function name must be quoted and the list
comma separated), but it gets the job done. You can only pass Qt objects into
QML, so to pass a value from Lisp into QML, you have to use the EQL5 bindings
to build a Qt object of the correct type. It would be nice if most Common Lisp
data types were automatically coerced into the appropriate C++ types like Cljfx
does from Clojure to Java.

The scope of this survey is limited to desktop GUI libraries, but there are
ports of EQL5 to Android and iOS, and a work-in-progress port to Sailfish OS,
so EQL5 can be used to develop mobile apps. QML being designed with touch
screens in mind might be a great fit on mobile. EQL5 can also be embedded in
existing C++ projects. I try to include a screenshot of an app developed with
each library, but I couldn't find any apps developed with EQL5, so I reached
out on Reddit, and the maintainer of EQL5 was nice enough to provide a
screenshot of an app used by a gym that was developed with EQL5 and QML. If you
really want to use Common Lisp instead of Racket or Clojure, EQL5 is a solid
choice, but I'd still recommend Racket or Cljfx if you're starting a new
project from scratch and can choose the language.

### LambdaNative

Links: [Tutorial](https://blog.matthewdmiller.net/learn-lambdanative-by-example-desktop-gui) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/lambdanative)<br>
**Lisp Dialect:** Scheme / **Scheme Implementation:** Gambit / **GUI Toolkit:** Custom (rendered with OpenGL)

<img align="right" width="480" height="320" src="https://raw.githubusercontent.com/goober99/lisp-gui-examples/master/screenshots/survey/lambdanative-phone-oximeter.jpg" />

Don't build a desktop app with LambdaNative. Let me be clear: My advice to
avoid this framework only applies to building desktop apps, which the scope of
this survey is limited to. LambdaNative is also capable of building Android,
iOS, and even BlackBerry apps (and also non-GUI apps for OpenWrt, but I can't
for the life of me think of why you would use LambdaNative to create a non-GUI
app instead of just using the underlying Gambit itself or one of a dozen other
Scheme implementations). I can't speak to LambdaNative's quality on mobile
since I haven't developed a mobile app using it myself yet, but its development
seems to be more oriented toward mobile. I hope to try it out for a mobile app
in the future.

According to the [LambdaNative website](https://www.lambdanative.org/), it is
developed and maintained by the Pediatric Anesthesia Research Team (PART) and
the Electrical Engineering in Medicine (ECEM) group at the University of
British Columbia, and it has been used in clinical trials of medical technology
in more than 10 countries involving around 100,000 subjects in all. I'm not
sure how many developer resources this arrangement provides, but it definitely
could use with a graduate assisstant or student being assigned to spend a
summer improving the documentation. Be prepared to read the source code of a
module (helpfully the documentation for each module links to the source code
implementing that module) to determine all the parameters and options for that
module. That's actually not the reason I recommend against using this framework
for a desktop app. The modules are written in very clear, easy-to-read Scheme,
and I assume if you're developing an app with Scheme that you know Scheme well
enough to figure it out.

LambdaNative falls into the camp of GUI toolkits that draws its own widgets
instead of using native widgets. It uses OpenGL for rendering. This is fraught,
because even seemingly simple widgets have multiple features such as
copy-and-paste and drag-and-drop that are needed in order for the widget to
behave as expected by users. As an example, I used a text field in the example
I implemented with LambdaNative. Here are just a few features I found lacking
from this one widget: I can't select the entire contents of the field and hit
delete or type to replace. I have to backspace each character individually. I
can't copy-and-paste into the field using Ctrl-V or drag-and-drop. When I
initially created the example, I couldn't input numbers using my keyboard's
numpad, only the row of numbers above the rows of letters. This has been fixed
in a subsequent version of LambaNative, and I'm now able to use my numpad. I
also used a dropdown box. My mouse wheel won't scroll through the options in
the dropdown. I have to drag the list up and down to scroll. Maybe the widgets
are more feature complete on mobile (I can't attest one way or the other), but
at least for desktop, unless you're developing an app that consists almost
entirely of custom widgets, I would steer far clear of LambdaNative.

Unlike most other GUI toolkits I reviewed, LambdaNative lacks any container
widgets for laying out other widgets in columns and rows. This wouldn't be so
bad except each widget must be layed out by pixel. The size and position
attributes of each widget only accept pixels, not percentages or other scalable
units you may be familiar with from CSS. This is especially strange for a
framework that advertises itself as a way to create both desktop and mobile
apps from a single codebase. Also, LambdaNative doesn't come with even a basic
or default theme. Most of the widgets have required parameters for things such
as color and font. You can't even create a slider without specifying the colors
of the elements of the slider. It would have been nice to have had some kind of
styling mechanism like a theme engine or CSS instead of being required to
specify basic style parameters for each individual widget.

I'm going to keep an eye on LambdaNative for mobile development but stay away
from it for desktop development.

### PS/Tk

Links: [Tutorial](https://blog.matthewdmiller.net/) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/pstk)<br>
**Lisp Dialect:** Scheme / **Scheme Implementation:** Chicken / **GUI Toolkit:** Tk

<img align="right" width="300" height="240" src="https://raw.githubusercontent.com/goober99/lisp-gui-examples/master/screenshots/survey/pstk-bintracker.png" />

While its egg form is specifically packaged for Chicken Scheme, PS/Tk actually
stands for a portable Scheme interface to the Tk GUI toolkit. It has a rich
history going all the way back to Scheme_wish by Sven Hartrumpf in 1997.
Wolf-Dieter Busch created a Chicken port called Chicken/Tk in 2004. It took on
its current name when Nils M Holm stripped it of Chicken-isms to make it
portable amongst Scheme implementations in 2006.

Tk also has a rich history as well. It has been around since 1991 and runs on a
wide variety of platforms. It was originally developed as an extension to Tcl
and is still developed alongside Tcl (thus why it is often referred to as
Tcl/Tk). Given how easy it is to package Tk for other scripting languages such
as Perl and Python, I can imagine an alternate reality where Tk is the dominant
cross-platform GUI toolkit. Alas, we don't live in that world.

One of the reasons for that is that Tk is just not all that pretty to look at.
For many years Tk emulated the look and feel of Motif, the dominant look and
feel of 1980s Unix workstations. It now has a theming engine. The default
themes on Windows and macOS supposedly do a decent job of approximating the
look of native widgets on those platforms. I don't use either of those
platforms, so I can't verify this first hand. For some reason, the default
theme on Linux is vaguely Windows 95ish. I tried a handful of themes that were
supposed to imitate the look and feel of popular GTK and Qt themes. They each
had glaring differences from the real thing that made them stick out like a
sore thumb.

The maintainer of the PS/Tk egg also develops a chiptune audio workstation
called [Bintracker](https://bintracker.org/) that uses PS/Tk for its GUI. He
went the path of developing a custom Tk theme just for Bintracker. That's a lot
of work to go to if all you want are some basic widgets that look decent on
your user's screen.

If you really want a GUI for some obscure implementation of Scheme, [Holm's
`pstk.scm`](http://mirror.informatimago.com/scheme/www.t3x.org/pstk/index.html)
should be portable to any implementation that is reasonably R5RS-compatible
without too much effort. It interfaces with Tk, not with the C library
bindings, but with a named pipe to `tclsh8.6`, and there is a configurable
section that begins with the string `NON-PORTABLE` in comments. The file
already comes with implementations for a dozen different Scheme implementations
(like Bigloo, Gauche, and Kawa) that can just be uncommented. For a quick GUI
for some utility you've written for your own use, Tk isn't that bad, but I
wouldn't use it for anything other people were going to see.
