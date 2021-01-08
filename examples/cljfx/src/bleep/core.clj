(ns bleep.core
  (:gen-class)
  (:require [cljfx.api :as fx])
  (:import [javafx.application Platform]))

; Scale used by slider
(def min-position 0)
(def max-position 2000)
; Range of frequencies
(def min-frequency 20)
(def max-frequency 20000)

; Cljfx global state
(def *state
  (atom {:frequency 440}))

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
; Update frequency in global state if it's a valid frequency
(defn set-frequency [freq]
  (when (and (>= freq min-frequency) (<= freq max-frequency))
    (swap! *state assoc :frequency freq)))

; Compose the UI map to be rendered by JavaFX
; Splitting it up makes it more readable since lines don't nest as deeply.

; Text field limited to entering numbers within range that updates specified
; key in global state atom (state-key)
(defn num-filter [change]
  (let [input (.getControlNewText change)
        numified (read-string input)]
    (if (or (= input "") (number? numified))
        change
        nil)))
(defn number-field [{:keys [min-value max-value init-value state-key]}]
  {:fx/type :text-field
   :text-formatter {:fx/type :text-formatter
                    :value-converter :number
                    :filter num-filter
                    :value init-value
                    :on-value-changed #(cond (< % min-value) (swap! *state assoc state-key min-value)
                                             (> % max-value) (swap! *state assoc state-key max-value)
                                             :else (swap! *state assoc state-key %))}})

; Frequency slider and controls
(defn frequency-slider [{:keys [frequency]}]
  {:fx/type :slider
   :min min-position
   :max max-position
   :value (frequency->position frequency)
   :on-value-changed #(set-frequency (position->frequency %))})
(defn octave-button [{:keys [frequency label modifier]}]
  {:fx/type :button
   :text label
   :on-action (fn [_] (set-frequency (* frequency modifier)))})
(defn frequency-controls [{:keys [frequency]}]
  {:fx/type :h-box
   :children [{:fx/type octave-button
               :frequency frequency
               :label "<"
               :modifier 0.5}
              {:fx/type :h-box
               :alignment :center
               :spacing 5
               :padding {:top 0 :bottom 0 :left 20 :right 20}
               :children [{:fx/type number-field
                           :min-value min-frequency
                           :max-value max-frequency
                           :init-value frequency
                           :state-key :frequency}
                          {:fx/type :label
                           :text "Hz"}]}
              {:fx/type octave-button
               :frequency frequency
               :label ">"
               :modifier 2}]})

; Main window
(defn root [{:keys [frequency]}]
  {:fx/type :stage
   :showing true
   :title "Bleep"
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :padding 25
                  :spacing 40
                  :children [{:fx/type frequency-slider
                              :frequency frequency}
                             {:fx/type frequency-controls
                              :frequency frequency}]}}})

; Renderer with middleware that maps incoming data to component description
(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)))

(defn -main [& args]
  (Platform/setImplicitExit true)
  (fx/mount-renderer *state renderer))
