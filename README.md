# Lisp GUI Examples

![Screenshot](screenshots/racket.png?raw=true "Racket screenshot")

I wanted to experiment with GUI programming in Lisp. I wanted to do something
more than a button that says "Click Me", and I had no desire to create a
calculator. I decided to create a GUI frontend for the Linux command line tool
[beep](https://github.com/johnath/beep) that can be used to control the PC
speaker.

Currently there is an implementation in Racket. I plan to port it to additional
Lisp dialects.

## Running the Example

Racket and beep are available in the repository of most Linux distros. Install
them from your distro's repo.

Then clone this repo and execute bleep.rkt with Racket. You'll probably have to
preface the command with `sudo` to actually control your PC speaker. See "A
note about ioctl" in the [README](https://github.com/johnath/beep) for beep.

```bash
git clone https://github.com/goober99/lisp-gui-examples.git
cd lisp-gui-examples
sudo racket bleep.rkt
```

## Supported Platforms
Since Linux is the platform I use and develop on, I have made no effort to make
these examples work on other platforms. If you feel so inclined, pull requests
porting the examples to other platforms will be accepted. Since beep is a Linux
tool, suffice it to say, any port to another platform would also require an
alternative to beep.

## License
[Artistic License 2.0](https://www.perlfoundation.org/artistic-license-20.html)
