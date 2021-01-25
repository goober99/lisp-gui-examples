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
requests. If you thinked I missed a library that I should try out, let me know.

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

Links: [Tutorial](https://blog.matthewdmiller.net/learn-racket-by-example-gui-programming) / [Code](https://github.com/goober99/lisp-gui-examples/tree/master/examples/racket)

<img align="right" src="../../screenshots/survey/racket-activitymonitor.png?raw=true" />

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
unmaintained.

There's even a WYSIWYG GUI designer for Racket ([MrEd
Designer](https://github.com/Metaxal/MrEd-Designer)) if you're so inclined. The
one thing on my wishlist that Racket lacks is a declarative syntax for
declaring GUIs, but Racket being Racket, it wouldn't be too difficult to build
a declarative DSL.
