Clojure is a dialect of Lisp that runs on the JVM. JavaFX is a modern GUI
toolkit for the JVM. You could use JavaFX directly with Clojure's Java interop,
but Cljfx provides a declarative and functional wrapper for JavaFX. Instead of
building yet another calculator, we're going to use Cljfx to build a GUI for
generating a tone.

![Screenshot](../../screenshots/racket.png?raw=true "Example screenshot")

You'll need Clojure installed. We also need a way to pull the Cljfx dependency
into our app. It seems [deps.edn](https://clojure.org/guides/deps_and_cli)
would be the easiest way to accomplish this for a short tutorial example such
as this, but I develop on Debian and found that the new [command line
tools](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=891141) for Clojure
that enable this haven't made it into Debian yet. I could install Clojure from
the Clojure website, but the point of this tutorial is to be easy to follow by
someone not familiar with Clojure. It's a lot easier to direct a person to `apt
install clojure` (or whatever package manager your preferred distro uses). This
tutorial is a living document maintained on GitHub along with the example code.
Whenever the Clojure command line tools become available on Debian, it's
possible I'll revisit this tutorial and update it.

The Clojure build tool Leiningen is capable of managing dependencies (and a
whole lot more). There is also a build tool for Clojure called Boot. I found
more plentiful tutorials and beginner resources for Leiningen, plus Leiningen
is available as a Debian package, so for this tutorial, Leiningen it is.
Install Clojure and Leiningen from your distro's repo. Debian also has an
OpenJFX package, but Leiningen was able to pull OpenJFX in as a dependency of
Cljfx. The version pulled in by Leiningen is probably more up to date than
whatever might be sitting in your distro's repos.

After that, we need to create a Leiningen project directory. This can be
accomplished with `lein new app bleep`, but that created a directory full of
subdirectories and files that were unnecessary for a short little example like
this. I created the directory structure (`mkdir -p cljfx/src/bleep`) and
created `cljfx/project.clj` and `cljfx/src/bleep/core.clj` with the minimum
needed to create a window with Cljfx.

`project.clj`
```clojure
(defproject bleep "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [cljfx "1.7.12"]]
  :main bleep.core)
```

`core.clj`
```clojure
(ns bleep.core
  (:gen-class)
  (:require [cljfx.api :as fx])
  (:import [javafx.application Platform]))

(defn hello-world [& args]
  {:fx/type :label
   :text "Hello, World!"})

(defn root [& args]
  {:fx/type :stage
   :showing true
   :title "Bleep"
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :padding 25
                  :spacing 40
                  :children [{:fx/type hello-world}]}}})

(def renderer
  (fx/create-renderer))

(defn -main [& args]
  (Platform/setImplicitExit true)
  (renderer {:fx/type root}))
```

The project file (`project.clj`) is the file that tells Leiningen about your
project (such as its dependencies). Typically with Leiningen, your code will
live in the `src` subdirectory in a subdirectory that matches the top-level
namespace of your project. All this can be changed with your `project.clj`, but
we'll be sticking to convention for this example. In this subdirectory is
`core.clj`.

Begin the file with a namespace declaration. The namespace should have a
`(:gen-class)` declaration in the `ns` form at the top for Leiningen. Since
we'll be using Cljfx to create our GUI, we need to `require` it. We'll mostly
be using JavaFX via Cljfx, but you can also access JavaFX classes directly.
Let's `import` (a way to get Java code into Clojure) the JavaFX Platform class.

Leiningen needs a `-main` function (note the leading dash is part of the
function name). This is the function that will get called when you run your app
with Leiningen or when you compile it to a standalone JAR. We use the
`setImplicitExit` method from the JavaFX Platform class to make sure the JavaFX
runtime shuts down when the window is closed.

Cljfx is declarative instead of imperative or object oriented like many GUI
libraries. You describe the layout with a map of key-value pairs instead of
creating controls with methods or functions. The `:fx/type` key has a value of
a kebab-cased keyword derived from a JavaFX class name. The other keys of the
map are the kebab-cased properties of that class. You can refer to the [JavaFX
documentation](https://openjfx.io/) for a list of classes and their properties.
JavaFX is well documented and has a large number of controls available.

JavaFX defines the user interface by means of a stage and a scene. The stage is
the top-level JavaFX container (the window). The scene is the container for all
content. To add a JavaFX class to the map, you convert the class name to
kebab-case. For example, JavaFX has a `VBox` pane used to lay out controls
vertically. This would be written `:v-box` in the Cljfx map. The `VBox` class
has a `padding` property. We can set this to 25 by adding a `:padding` key to
the map with a value of 25.

Cljfx provides a renderer function created with `fx/create-renderer` that you
pass the map describing your user interface to. You can call `renderer`
multiple times to dynamically change the GUI. Now let's replace the Hello World
label with a slider:

```clojure
(defn frequency-slider [{:keys [frequency]}]
  {:fx/type :slider
   :min 20
   :max 20000
   :value frequency})
```

Instead of placing the definition of your entire UI into one big map, you can
compose the UI from reusable functions. This also makes your code more
readable, because you don't have a deeply nested map. These functions can be
used just like any other JavaFX class. The `:fx/type` key is the function name,
and the other keys of the map are the arguments to the function. We can add the
`frequency-slider` we created above to the `:children` vector of the `:v-box`
in the `root` map like this:

```clojure
[{:fx/type frequency-slider
  :frequency 440}]
```

The range of frequencies audible by humans is typically between 20 Hz and 20
KHz (we lose the ability to hear some of those higher frequencies as we age).
The [musical note A above middle
C](https://en.wikipedia.org/wiki/A440_(pitch_standard)) is 440 Hz. Since A4
serves as a general tuning standard, it seems like a sensible default, but if
you run the above with `lein run`, this is what you'll see:

![Slider](../../screenshots/cljfx-linearslider.png?raw=true "Slider showing 440 using a linear scale")

The scale of 20 to 20,000 is so large that 440 doesn't appear to move the
slider at all. Ideally, 440 would fall about the middle of the slider. To
achieve this, let's use a logarithmic scale.

I found a [Stack Overflow
answer](https://stackoverflow.com/questions/846221/logarithmic-slider/846249#846249)
on how to map a slider to a logarithmic scale. The code given in the answer is
JavaScript, but it was easy enough to port to Clojure.

```clojure
; Scale used by slider
(def min-position 0)
(def max-position 2000)
; Range of frequencies
(def min-frequency 20)
(def max-frequency 20000)

; Logarithmic scale for frequency (so middle A [440] falls about in the middle)
; Adapted from https://stackoverflow.com/questions/846221/logarithmic-slider

(def min-freq (Math/log min-frequency))
(def max-freq (Math/log max-frequency))
(def frequency-scale (/ (- max-freq min-freq) (- max-position min-position)))
; Convert slider position to frequency
(defn position->frequency [position]
  (int (Math/round (Math/exp (+ min-freq (* frequency-scale (- position min-position)))))))
; Convert frequency to slider position
(defn frequency->position [freq]
  (int (Math/round (/ (- (Math/log freq) min-freq) (+ frequency-scale min-position)))))
```

I added some global parameters to the top of the script. I came up with the
range of 0-2,000 by trial and error. It seemed to strike the best balance
between each step of the slider making a noticeable change to the frequency
while still allowing the user to narrow in on a specific frequency with just
the slider.

Then we create two functions: one that takes the position on the slider and
returns the frequency (`position->frequency`) and another that takes a
frequency and returns the position on the slider (`frequency-position`). Now
let's set the initial position of our slider with the `frequency->position`
function:

```clojure
(defn frequency-slider [{:keys [frequency]}]
  {:fx/type :slider
   :min min-position
   :max max-position
   :value (frequency->position frequency)})
```

Underneath the slider is a text field showing the current frequency.

```clojure
(defn frequency-controls [{:keys [frequency]}]
  {:fx/type :h-box
   :alignment :center
   :spacing 20
   :children [{:fx/type :h-box
               :alignment :center
               :spacing 5
               :children [{:fx/type :text-field
                           :text (str frequency)}
                          {:fx/type :label
                           :text "Hz"}]}]})
```

Add this to the `:children` vector of the `:v-box` in the `root` map:

```clojure
[{:fx/type frequency-slider
  :frequency frequency}
 {:fx/type frequency-controls
  :frequency frequency}]
```

At this point, we are starting to have a nice looking interface, but it doesn't
do anything. If you slide the slider, nothing happens. If your experience is
mostly with object-oriented GUI libraries, the way Cljfx does things will be a
little unfamiliar. If you've used the JavaScript library React, it employs a
similar model. Global state is stored in an atom. When you update that atom,
all the relevant controls are updated accordingly.

```clojure
; Cljfx global state
(def *state
  (atom {:frequency 440}))
```

Add middleware to the `renderer` that maps incoming data from the global state
atom to the component description to be rendered:

```clojure
(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)))
```

Then modify `-main` to use `fx/mount-renderer` instead of calling `renderer`
directly. This will watch the global state atom for changes and rerender the
GUI accordingly.

```clojure
(defn -main [& args]
  (Platform/setImplicitExit true)
  (fx/mount-renderer *state renderer))
```

In most of the other examples, I would now write a function for each control
that updates the other controls when its value changes. For example, the slider
must update the text field, and the text field must update the slider. Imagine
in the future I decide to add a meter to the UI that also needs to be updated
whenever the frequency changes. I would have to remember to add code to update
the meter to both the slider and the text field. With Cljfx, the slider just
has to update the value in the global state atom. Then the text field and any
controls we add in the future that depend on the frequency will automatically
update. Here's a function that will update the global state atom:

```clojure
; Update frequency in global state if it's a valid frequency
(defn set-frequency [freq]
  (when (and (>= freq min-frequency) (<= freq max-frequency))
    (swap! *state assoc :frequency freq)))
```

Then we can add an `:on-value-changed` property to the `:slider` that calls
this function:

```clojure
:on-value-changed #(set-frequency (position->frequency %))
```

and add an `:on-text-changed` property to the `:text-field`:

```clojure
:on-text-changed #(set-frequency (read-string %))
```

And that's it. When one changes, the other does too. Once you get your head
around it, it's a really elegant way to handle UI state.

It might be helpful to have a couple buttons to increase or decrease the
frequency by an octave. An [octave](https://en.wikipedia.org/wiki/Octave) is
"the interval between one musical pitch and another with double its frequency."

```scheme
(defn octave-button [{:keys [frequency label modifier]}]
  {:fx/type :button
   :text label
   :on-action (fn [_] (set-frequency (* frequency modifier)))})
```

Now just add these buttons to the `:children` vector of the `:h-box` in the
`frequency-controls` map:

```clojure
[{:fx/type octave-button
  :frequency frequency
  :label "<"
  :modifier 0.5}
 {:fx/type :h-box
  :alignment :center
  :spacing 5
  :padding {:top 0 :bottom 0 :left 20 :right 20}
  :children [{:fx/type :text-field
              :text (str frequency)
              :on-text-changed #(set-frequency (read-string %))}
             {:fx/type :label
              :text "Hz"}]}
 {:fx/type octave-button
  :frequency frequency
  :label ">"
  :modifier 2}]
```

If you slide the slider, the text field updates accordingly. If you type a
number in the text field, the slider updates accordingly. All good, right? What
if a user (and you know they will) enters a number higher than 20,000 or a
letter?

JavaFX has a `TextFormatter` class for this purpose. The `TextFormatter` uses a
filter that can intercept and modify user input and a value converter that
converts the value of the text field to a specified type whenever the field
loses focus or the user hits Enter. The filter should be a function that
accepts a Java `TextFormatter.Change` object and either returns that object or
returns null (`nil` in Clojurese) to reject the change.

```scheme
; Text field limited to entering numbers within range that updates specified
; key in global state atom (state-key)
(defn num-filter [change]
  (let [input (.getControlNewText change)
        numified (read-string input)]
    (if (or (= input "") (number? numified))
        change
        nil)))
(defn number-field [{:keys [min-value max-value init-value state-key label]}]
  {:fx/type :h-box
   :alignment :center
   :spacing 5
   :padding {:top 0 :bottom 0 :left 20 :right 20}
   :children [{:fx/type :text-field
               :pref-column-count (+ 1 (count (str max-value)))
               :text-formatter {:fx/type :text-formatter
               :value-converter :number
               :filter num-filter
               :value init-value
               :on-value-changed #(cond (< % min-value) (swap! *state assoc state-key min-value)
                                        (> % max-value) (swap! *state assoc state-key max-value)
                                        :else (swap! *state assoc state-key %))}}
              {:fx/type :label
               :text label}]})
```

Cljfx doesn't provide wrappers for the methods of `TextFormatter.Change`, but
we can call the `getControlNewText()` method using Clojure's Java interop. This
method returns the complete new text wich will be used on the control after the
change. We can check this value to determine if we want to accept the change or
not.

A caveat here is that the value of `:filter` [must
be](https://github.com/cljfx/cljfx/issues/114#issuecomment-754642403) a
top-level️ function. I wish I could define `:filter` as an anonymous function.
This would allow me to do something like also preventiing the user from even
entering a number higher than the allowed range rather than just reverting to
the maximum value once the value is committed.

```clojure
: filter #(let [input (.getControlNewText %])
                numified (read-string input)]
            (if (or (= input "") (and (number? numified) (<= max-value)))
                %
                nil))
```

If you try the above, it will throw a `Replace forbidden` error. That's because
`:filter` is a constructor argument of `TextFormatter` that cannot be modified
afterward. If you use an anonymous function, every call to `number-field`
(whenever the UI is being rerendered due to a change of state) will try to
create a new instance of the filter function, which is forbidden. But by making
`num-filter` a top-level function, it is now out of the scope of `max-value`. I
tried passing `max-value` to it by wrapping `num-filter` with a function that
takes `max-value` and calling that function in `:filter`, but that still
resulted in `Replace forbidden`. I also tried defining `max-value` at the
top-level and wrapping the map inside `number-field` with `binding` to reassign
the value of `max-value`, but since `num-filter` is being passed to the
`TextFormatter` and called later it is called outside the scope of the
`binding` and still unable to see `max-value`. I finally gave up and fell back
on just having the field revert to `max-value` once the value is committed. If
anyone has a solution for getting `max-value` to the `num-filter` function, I
welcome pull requests.

Now let's replace the baseline `:text-field` and the `:h-box` surrounding it
with our new `number-field`:

```clojure
{:fx/type number-field
 :min-value min-frequency
 :max-value max-frequency
 :init-value frequency
 :state-key :frequency
 :label "Hz"}
```

Let's use this `number-field` again to create a field to specify the duration
of the beep in milliseconds. First, let's add a key to our global state atom to
track duration:

```clojure
(def *state
  (atom {:frequency 440
         :duration 200}))
```

Then create our duration field:

```clojure
(defn general-controls [{:keys [duration]}]
  {:fx/type :h-box
   :spacing 20
   :children [{:fx/type number-field
               :min-value 1
               :max-value 600000 ; 10 minutes
               :init-value duration
               :state-key :duration
               :label "ms"}]})
```

Frequency is rather abstract. Let's also give the user the ability to select a
musical note. We can store the corresponding frequencies for A4-G4 in a map.

```clojure
; Notes -> frequency (middle A-G [A4-G4])
; http://pages.mtu.edu/~suits/notefreqs.html
(def notes {"A" 440.00
            "B" 493.88
            "C"261.63
            "D" 293.66
            "E" 329.63
            "F" 349.23
            "G" 292.00})
```

We'll give the user a drop-down menu. Whenever a note is selected from the
drop-down menu, we'll look up the frequency in the map and set that frequency
in the global state atom. We'll add it on to `general-controls`:

```clojure
(defn general-controls [{:keys [duration]}]
  {:fx/type :h-box
   :spacing 20
   :children [{:fx/type number-field
               :min-value 1
               :max-value 600000 ; 10 minutes
               :init-value duration
               :state-key :duration
               :label "ms"}
              {:fx/type :h-box
               :alignment :center
               :spacing 5
               :children [{:fx/type :label
                           :text "♪"}
                          {:fx/type :choice-box
                           :items ["A" "B" "C" "D" "E" "F" "G"]
                           :on-value-changed #(set-frequency (notes %))}]}]})
```

Finally, let's make some noise.
