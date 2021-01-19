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

## Cljfx (Clojure/JavaFX)
