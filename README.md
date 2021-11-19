# Lisp GUI Examples

I wanted to experiment with GUI programming in Lisp. I wanted to do something
more than a button that says "Click Me", and I had no desire to create a
calculator or to-do list. I decided to create a GUI for generating a tone.

## Running the Examples

The examples are housed in the `examples` subdirectory. Each example has its
own subdirectory. To run the examples, clone this repo and navigate to the
`examples` subdirectory.

```console
$ git clone https://github.com/goober99/lisp-gui-examples.git
$ cd lisp-gui-examples/examples
```

### cl-cffi-gtk (SBCL Common Lisp/GTK)

![Screenshot](screenshots/cl-cffi-gtk.png?raw=true "cl-cffi-gtk screenshot")

SBCL is available in the repository of most Linux distros. Depending on your
desktop environment, you probably already have GTK installed (even if you use
KDE, it's likely you already have GTK installed). For Debian/Ubuntu:

```console
$ sudo apt install sbcl
```

The example uses Quicklisp to load cl-cffi-gtk and CL-PortAudio. Install
Quicklisp and configure SBCL.

Navigate to the `cl-cffi-gtk` example subdirectory and execute bleep.lisp with
SBCL.

```console
$ cd cl-cffi-gtk
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load quicklisp.lisp
> (quicklisp-quickstart:install)
> (ql:add-to-init-file)
> (exit)
$ sbcl --load bleep.lisp
```

### Cljfx (Clojure/JavaFX)

![Screenshot](screenshots/cljfx.png?raw=true "Cljfx screenshot")

Clojure and Leiningen are available in the repository of most Linux distros.
Install them from your distro's repo. For Debian/Ubuntu:

```console
$ sudo apt install clojure leiningen
```

Navigate to the `cljfx` example subdirectory and run the project with
Leiningen.

```console
$ cd cljfx
$ lein run
```

### EQL5 (ECL Common Lisp/Qt)

![Screenshot](screenshots/eql5.png?raw=true "EQL5 screenshot")

ECL and Qt are available in the repository of most Linux distros, but you'll
probably need to compile EQL5 from source. For instruction on compiling EQL5,
see the [EQL5
tutorial](https://github.com/goober99/lisp-gui-examples/blob/master/examples/eql5/tutorial.md#compiling-eql5).

The example uses CL-PortAudio to generate the tone. Install Quicklisp to load
CL-PortAudio.

After compiling EQL5 from source, navigate to the `eql5` example subdirectory
and execute bleep.lisp with EQL5.

```console
$ cd eql5
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ ecl -load quicklisp.lisp
> (quicklisp-quickstart:install)
> (ql:add-to-init-file)
> (quit)
$ eql5 bleep.lisp
```

### LambdaNative (Gambit Scheme)

![Screenshot](screenshots/lambdanative.png?raw=true "LambdaNative screenshot")

For instruction on setting up LambdaNative, see the [LambdaNative
tutorial](https://github.com/goober99/lisp-gui-examples/blob/master/examples/lambdanative/tutorial.md#installing-lambdanative).

Navigate to the `lambdanative` example subdirectory. Copy `apps/bleep` into
your LambdaNative apps directory. Then from your LambdaNative root directory,
compile the example.

```console
$ cd lambdanative
$ cp -r apps/bleep ~/lambdanative/apps
$ ./configure bleep
$ make
$ make install
```

### PS/Tk (Chicken Scheme/Tk)

![Screenshot](screenshots/pstk.png?raw=true "PS/Tk screenshot")

Chicken Scheme, Tcl/Tk, and Allegro are available in the repository of most
Linux distros. Install it from your distro's repo. For Debian/Ubuntu:

```console
$ sudo apt install chicken-bin tcl tk liballegro5-dev
```

Optionally on Linux, you can install additional Tk themes and apply the theme
of your choice system wide. With MATE on Debian, I prefer Arc (the screenshot
above is with the Arc theme). KDE users might prefer Breeze while Ubuntu users
might opt for Yaru.

```console
$ sudo apt install tcl-ttkthemes
$ echo '*TkTheme: arc' | xrdb -merge -
```

The example uses the Allegro egg to generate the tone. You can install it and
the PS/Tk egg with the `chicken-install` utility that comes with Chicken
Scheme.

Navigate to the `pstk` example subdirectory and execute bleep.scm with Chicken
Scheme.

```console
$ chicken-install -sudo pstk allegro
$ cd pstk
$ csi bleep.scm
```

### Racket

![Screenshot](screenshots/racket.png?raw=true "Racket screenshot")

Racket is available in the repository of most Linux distros. Install it from
your distro's repo. For Debian/Ubuntu:

```console
$ sudo apt install racket
```

The example uses the RSound package to generate the tone. You can install it
with the `raco` utility that comes with Racket.

Navigate to the `racket` example subdirectory and execute bleep.rkt with
Racket.

```console
$ raco pkg install rsound
$ cd racket
$ racket bleep.rkt
```

## Supported Platforms
Since I exclusively use Linux, I have only tested these examples on Linux. If
you find problems on a different platform, pull requests are welcome to improve
cross-platform compatibility.

## License
[Artistic License 2.0](https://www.perlfoundation.org/artistic-license-20.html)
