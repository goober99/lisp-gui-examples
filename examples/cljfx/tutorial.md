Clojure is a dialect of Lisp that runs on the JVM. JavaFX is a modern GUI
toolkit for the JVM. You could use JavaFX directly with Clojure's Java interop,
but Cljfx provides a declarative and functional wrapper for JavaFX. Instead of
building yet another calculator, we're going to use Cljfx to build a GUI for
generating a tone.

![Screenshot](../../screenshots/cljfx.png?raw=true "Example screenshot")

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
                 [cljfx "1.7.12"]
                 [com.jsyn/jsyn "20170815"]]
  :main bleep.core)
```

`core.clj`
```clojure
(ns bleep.core
  (:gen-class)
  (:require [cljfx.api :as fx])
  (:import [javafx.application Platform]
           [com.jsyn JSyn]
           [com.jsyn.unitgen LineOut SineOscillator]))

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
We'll also import the Java JSyn library for use later in generating the tone.

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

Underneath the slider is a spinner showing the current frequency.

```clojure
(defn number-field [{:keys [value-factory-class min-value max-value init-value state-key label]}]
  {:fx/type :h-box
   :alignment :center
   :spacing 5
   :children [{:fx/type :spinner
               :editable true
               :value-factory {:fx/type value-factory-class
                               :min min-value
                               :max max-value
                               :value init-value}}
              {:fx/type :label
               :text label}]})

(defn frequency-controls [{:keys [frequency]}]
  {:fx/type :h-box
   :alignment :center
   :spacing 20
   :children [{:fx/type number-field
               :value-factory-class :double-spinner-value-factory
               :min-value min-frequency
               :max-value max-frequency
               :init-value frequency
               :state-key :frequency
               :label "Hz"}]})
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

and add an `:on-value-changed` property to the `:spinner`:

```clojure
:on-value-changed #(swap! *state assoc state-key %)
```

And that's it. When one changes, the other does too. Once you get your head
around it, it's a really elegant way to handle UI state.

It might be helpful to have a couple buttons to increase or decrease the
frequency by an octave. An [octave](https://en.wikipedia.org/wiki/Octave) is
"the interval between one musical pitch and another with double its frequency."

```clojure
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

Let's use another spinner to specify the duration of the beep in milliseconds.
First, let's add a key to our global state atom to track duration:

```clojure
(def *state
  (atom {:frequency 440
         :duration 200}))
```

Then create our duration field:

```clojure
(defn general-controls [{:keys [frequency duration]}]
  {:fx/type :h-box
   :spacing 20
   :children [{:fx/type number-field
               :value-factory-class :integer-spinner-value-factory
               :min-value 1
               :max-value 600000 ; 10 minutes
               :init-value duration
               :state-key :duration
               :label "ms"}]})
```

Add this to the `:children` vector of the `:v-box` in the `root` map:

```clojure
[{:fx/type frequency-slider
  :frequency frequency}
 {:fx/type frequency-controls
  :frequency frequency}
 {:fx/type general-controls
  :frequency frequency
  :duration duration}]
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
(defn general-controls [{:keys [frequency duration]}]
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

```clojure
; Generate a tone using JSyn
; Adapted from https://github.com/daveyarwood/javasynth/blob/master/src/javasynth/getting_started.clj
(defn generate-tone [frequency duration amplitude]
  (let [synth (doto (. JSyn createSynthesizer) .start)
        out (LineOut.)
        sine (SineOscillator. frequency)]
    (.set (. sine -amplitude) amplitude)
    (.add synth out)
    (.add synth sine)
    (.connect (. sine -output) (. out -input))
    (let [now (. synth getCurrentTime)]
      (.start out)
      (. synth (sleepUntil (+ now (/ duration 1000))))
      (.stop synth))))
```

We'll use the Java [JSyn library](http://softsynth.com/jsyn/index.php) we
imported at the very beginning to generate the tone. Leiningen will pull in
this dependency alongside Cljfx. Java has a standard [Sound
API](https://openjdk.java.net/groups/sound/) that could be used to generate a
sine tone without any external dependencies, but it's a little too low-level
for what I'm trying to accomplish with this tutorial. This is a tutorial about
building a GUI with Clojure and not about sound processing with Clojure. If
you're interested, I did find a [well-written
tutorial](https://brainshave.com/blog/sound-synthesis/) about using Clojure's
Java interop to generate a sine tone with Clojure via the Java Sound API. Wire
this function up to a button between the duration and note selector, and you're
ready to make some noise.

```clojure
(defn general-controls [{:keys [frequency duration]}]
  {:fx/type :h-box
   :spacing 20
   :children [{:fx/type number-field
               :min-value 1
               :max-value 600000 ; 10 minutes
               :init-value duration
               :state-key :duration
               :label "ms"}
              {:fx/type :button
               :text "Play"
               :on-action (fn [_] (generate-tone frequency duration 0.5))}
              {:fx/type :h-box
               :alignment :center
               :spacing 5
               :children [{:fx/type :label
                           :text "♪"}
                          {:fx/type :choice-box
                           :items ["A" "B" "C" "D" "E" "F" "G"]
                           :on-value-changed #(set-frequency (notes %))}]}]})
```
