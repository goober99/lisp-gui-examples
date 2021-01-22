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

### Cljfx (Clojure/JavaFX)

![Screenshot](screenshots/cljfx.png?raw=true "Cljfx screenshot")

Clojure and Leiningen are available in the repository of most Linux distros.
Install them from your distro's repo.

Navigate to the `cljfx` example subdirectory and run the project with
Leiningen.

```console
$ cd cljfx
$ lein run
```

### LambdaNative (Gambit Scheme)

![Screenshot](screenshots/lambdanative.png?raw=true "LambdaNative screenshot")

Navigate to the `lambdanative` example subdirectory and execute the `bleep`
executable.

```console
$ cd lambdanative
$ ./bleep
```

To build the example from source, see the README in the `lambdanative` example
directory.

### Racket

![Screenshot](screenshots/racket.png?raw=true "Racket screenshot")

Racket is available in the repository of most Linux distros. Install it from
your distro's repo. The example uses the RSound package to generate the tone.
You can install it with the `raco` utility that comes with Racket.

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
