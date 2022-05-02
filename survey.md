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

**Links:** [Tutorial](https://dev.to/goober99/learn-racket-by-example-gui-programming-3epm) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/racket)<br>
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
(GTK on Linux).

While not as extensive as the widgets available if you were using the native
toolkits directly, Racket has all the [basic
widgets](https://docs.racket-lang.org/gui/Widget_Gallery.html) plus a [graph
plotting library](https://docs.racket-lang.org/plot/) and a [drawing
toolkit](https://docs.racket-lang.org/draw/index.html). Take a look at the
brilliant Alex Harsányi's
[ActivityLog2](https://github.com/alex-hhh/ActivityLog2), an app to analyze
data from various fitness activities, for an example of a complex GUI written
in Racket.

If you stumbled upon this article looking for a cross-platform GUI library for
another language, it might be worth it to switch to Racket. If Lisp is the
hidden gem of programming languages, Racket just may be the hidden gem of
cross-platform GUI toolkits. No, really. You can stop reading here. Unless you
have specific requirements, Racket is the best cross-platform GUI toolkit for
Lisp (or any language).

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

Links: [Tutorial](https://dev.to/goober99/learn-clojure-by-example-javafx-gui-with-cljfx-2f3b) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/cljfx)<br>
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

### cl-cffi-gtk

Links: [Tutorial](https://dev.to/goober99/learn-common-lisp-by-example-gtk-gui-with-sbcl-5e5c) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/cl-cffi-gtk)<br>
**Lisp Dialect:** Common Lisp / **Common Lisp Implementation:** SBCL (and probably others) / **GUI Toolkit:** GTK

<img align="right" width="320" height="254" src="https://raw.githubusercontent.com/goober99/lisp-gui-examples/master/screenshots/survey/clcffigtk-nyxt.png" />

GTK is a cross-platform GUI toolkit, but it is most well known on Linux where
multiple desktop environments are based on it (such as GNOME, Xfce, and MATE).
It draws its own widgets, but those widgets are the native widgets on most
Linux distros. It may look a bit foreign on Windows and macOS, but I don't
think native look and feel is as big of deal as some make it out to be.

The Common Lisp bindings are developed with SBCL but also tested with Clozure
CL and CLISP. As someone who generally prefers Scheme, one thing from Common
Lisp that I am jealous of is Quicklisp. Libraries in Quicklisp generally work
with most Common Lisp implementations. That can't be said for most Scheme
libraries, especially ones that require FFI.

One thing I really liked about cl-cffi-gtk is that it runs the GUI in a
separate thread. This is cool, because even after the window appears, you can
still type commands into the REPL to interact with the program. I could query
and even change the properties of widgets from the REPL. With most of the Lisp
GUI libraries I've tried out, the GUI takes over completely once it is
launched, and you have to close the window before being able to type commands
into the REPL again.

The [documentation](http://www.crategus.com/books/cl-gtk/gtk-tutorial.html) for
cl-cffi-gtk is filled with examples, tutorials, and helpful hints. It has some
of the best documentation of any Lisp GUI library I've evaluated.

The Git repo went without any commits from 2016-2019. This led many to believe
the project was abandoned and to a
[fork](https://github.com/crategus/cl-cffi-gtk/issues/77). The fork is now
marked as "archived by the owner" on GitHub while the maintainer of the
original has been quite active since 2019. There are fixes and changes in the
fork that were never merged back into the upstream, and the two have diverged
significantly now. At least as of this writing in 2021, Quicklisp still points
to the fork.

There's a WYSIWYG GUI designer for GTK called
[Glade](https://en.wikipedia.org/wiki/Glade_Interface_Designer). It outputs
XML, so it is programming language agnostic. There is an [example
application](https://github.com/ralph-schleicher/atmosphere-calculator) of a
GUI built with Glade and loaded with cl-cffi-gtk.

### EQL5

Links: [Tutorial](https://dev.to/goober99/learn-common-lisp-by-example-qt-gui-with-eql5-1lmn) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/eql5)<br>
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

### IUP

Links: [Tutorial](https://dev.to/goober99) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/iup)<br>
**Lisp Dialect:** Scheme / **Scheme Implementation:** Chicken / **GUI Toolkit:** Native (GTK on Linux)<br>
**Lisp Dialect:** Common Lisp / **Bindings:** lispnik/iup

<img align="right" width="320" height="324" src="https://raw.githubusercontent.com/goober99/lisp-gui-examples/master/screenshots/survey/iup-megatest.png" />

IUP is a cross-platform GUI toolkit that uses native controls. It uses GTK on
Linux and, well, GTK on macOS too. According to the
[roadmap](https://www.tecgraf.puc-rio.br/iup/en/to_do.html), there is a macOS
native driver in the works, but it looks like one of those perpetually "coming
soon" features. The last several releases didn't come with any pre-compiled
binaries for macOS either. I'm not sure if there is an issue preventing recent
releases from building on macOS or if they just don't have any developers with
macOS to contribute a macOS build. If macOS is one of the platforms you're
targeting, IUP would not be the toolkit to choose.

A Lisp developer criticizing a project for lack of widespread adoption is like
throwing rocks in a glass house, but IUP has been in development since the
1990s yet doesn't appear to be in the repos of any major Linux distro. It is
liberally licensed under the MIT license, so you might be able to static link
it and include it with your app but evaluating how feasible or difficult that
would have been with Chicken Scheme was outside the scope of my quick tutorial.

IUP supports a declarative language called
[LED](https://www.tecgraf.puc-rio.br/iup/en/led.html) for defining your UI, but
the Chicken egg provides a way to specify your entire UI as an S-expression
which is much more elegant than the somewhat awkward syntax of LED. IUP comes
with good [documentation](https://www.tecgraf.puc-rio.br/iup/), but all the
examples are in C and Lua.

There are also [Common Lisp bindings for IUP](https://github.com/lispnik/iup/).
They are described as pre-alpha but appear robust and come with numerous
examples. Introspection is used to automatically generate the bindings. The
latest version of IUP the bindings are compatible with is 3.29. IUP is not a
particularly fast moving project. The latest version of is IUP 3.30, released
on July 30, 2020 (almost two years ago at the time of this writing). Either the
automatic generation process moving from 3.29 to 3.30 is not that automatic or
the maintainer has lost interest.

Given the limited number of platforms it supports (support for macOS is iffy at
best), I don't see any compelling reason to choose IUP over any of the other
toolkits in this survey. It doesn't even have the ubiquity of Tk going for it
which you could probably get to work on just about any platform with any Lisp
implementation in a pinch.

### LambdaNative

Links: [Tutorial](https://dev.to/goober99/learn-lambdanative-by-example-desktop-gui-277l) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/lambdanative)<br>
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

### Tk

Links: [Tutorial](https://dev.to/goober99/learn-scheme-by-example-tk-gui-with-chicken-scheme-3cn9) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/pstk)<br>
**Lisp Dialect:** Scheme / **Scheme Implementation:** Chicken (and most other R5RS-compatible implementations) / **Bindings:** PS/Tk<br>
**Lisp Dialect:** Common Lisp / **Bindings:** LTK and nodgui

<img align="right" width="320" height="256" src="https://raw.githubusercontent.com/goober99/lisp-gui-examples/master/screenshots/survey/pstk-bintracker.png" />

While its egg form is specifically packaged for Chicken Scheme, Tk can probably
run on more Lisp implementations and platforms than any other GUI toolkit.
PS/Tk actually stands for a portable Scheme interface to the Tk GUI toolkit. It
has a rich history going all the way back to Scheme_wish by Sven Hartrumpf in
1997. Wolf-Dieter Busch created a Chicken port called Chicken/Tk in 2004. It
took on its current name when Nils M Holm stripped it of Chicken-isms to make
it portable amongst Scheme implementations in 2006.

There are two Tk bindings for Common Lisp:
[LTK](http://www.peter-herth.de/ltk/) and
[nodgui](https://www.autistici.org/interzona/nodgui.html). Both interface with
Tk over `wish` similar to how PS/Tk uses `tclsh` instead of C bindings. I
didn't build examples using either of the Common Lisp bindings because of the
similarities of the implementations, plus all my complaints about Tk would
remain regardless of whether using Scheme or Common Lisp.

Tk has a rich history. It has been around since 1991 and runs on a wide variety
of platforms. It was originally developed as an extension to Tcl and is still
developed alongside Tcl (thus why it is often referred to as Tcl/Tk). Given how
easy it is to package Tk for other scripting languages such as Perl and Python,
I can imagine an alternate reality where Tk is the dominant cross-platform GUI
toolkit. Alas, we don't live in that world.

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
already comes with implementations for over a dozen different Scheme
implementations (like Bigloo, Gauche, and Kawa) that can just be uncommented.
For a quick GUI for some utility you've written for your own use, Tk isn't that
bad, but I wouldn't use it for anything other people were going to see.

## Deprecated, Dormant, or Otherwise Out of Scope

You may have read the above survey and asked, "What about [insert GUI
library]?" I've only attempted to evaluate those GUI libraries I consider
production-quality. Here are some libraries that are not there yet (or maybe
not there anymore). I plan to keep this survey up to date, so I'll keep an eye
on these libraries, and if their status changes, you just might see them move
up above this heading in the survey.

- [CAPI](http://www.lispworks.com/products/capi.html) for LispWorks. Wraps
  native controls for Windows macOS, and Linux (GTK). Proprietary.
- [CLOG](https://github.com/rabbibotton/clog) for Common Lisp. Bills itself as
  a GUI library, but from what I can tell, really is just a web framework. You
  can package your web app to look like a desktop app with Electron or other
  webview, but that's no different than what you can do with other web
  frameworks, and evaluating web frameworks is outside the scope of this survey.
- [fn(fx)](https://github.com/fn-fx/fn-fx). Clojure wrapper of JavaFX. Last
  commit in 2019, and all GitHub issues have gone unanswered since then. See
  Cljfx, which is one of my two recommended GUI libraries, for a maintained
  Clojure wrapper of JavaFX.
- [Gauche-gtk](https://github.com/shirok/Gauche-gtk2). GTK bindings for Gauche
  Scheme. [Still depends](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=967370)
  on GTK 2. Debian package [fails to build](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=981928)
  and was removed from testing and thus current stable.
- [Guile-Gnome](https://www.gnu.org/software/guile-gnome/). GTK bindings for
  Guile. Essentially unmaintained. Still depends on [GTK
  2](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=967505) and [Guile
  2.2](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=969698). Last release
  was in 2017, and the release notes stated, "But don't hail, this is a
  maintainance release, which actually merely comment the build of the Corba and
  Gnome-VFS modules, due to incompatible changes in Guile-2.2. [I]f you think you
  have the skill and some free time to fix these two, please do!" I'm keeping an
  eye on [Guile GI](https://github.com/spk121/guile-gi) as a possible successor,
  but the developer still considers it beta quality and only partially
  documented, and the API is still subject to change.
- [McCLIM](https://common-lisp.net/project/mcclim/) for Common Lisp. This looks
  really interesting, and I'll be keeping an eye on it. Currently, the only
  officially supported [backend](https://github.com/McCLIM/McCLIM/wiki/Backends)
  renders directly to an X server and has a Motif look and feel. Until another
  backend matures that either wraps native controls or draws more modern looking
  controls using OpenGL, I don't consider it production ready.
- [Scheme Widget Library](https://www.scheme.com/swl.html). Tk bindings for
  Chez Scheme. Unmaintained.
- [Seesaw](https://github.com/clj-commons/seesaw). Clojure wrapper of Swing.
  Essentially [unmaintained](https://github.com/clj-commons/seesaw/issues/224).
  The original maintainer lost interest, and it was migrated to the [CLJ
  Commons](https://github.com/clj-commons) GitHub organization. They're stated
  goal is to "adopt important Clojure libraries when the original maintainers no
  longer have the time or interest to keep them updated." Since they adopted
  Seesaw, there hasn't been a single new commit. Swing itself is somewhat of a
  legacy library now that there is JavaFX.
