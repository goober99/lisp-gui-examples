;; bleep - GUI frontend for beep made with LambdaNative

(define gui #f)

;; UI color palette
(define *background-color* (color-rgb 26 26 29))
(define *foreground-color* (color-rgb 195 7 63))
(define *accent-color* (color-rgb 111 34 50))
(define *text-color* (color-rgb 255 255 255))
;; Scale used by slider
(define *min-position* 0)
(define *max-position* 2000)
;; Range of frequencies
(define *min-frequency* 1)
(define *max-frequency* 19999)
;; Notes -> frequency (middle A-G [A4-G4])
;; http://pages.mtu.edu/~suits/notefreqs.html
(define notes (list->table '((0 . 440.00)    ; A
                             (1 . 493.88)    ; B
                             (2 . 261.63)    ; C
                             (3 . 293.66)    ; D
                             (4 . 329.63)    ; E
                             (5 . 349.23)    ; F
                             (6 . 292.00)))) ; G

;; Register C-side real-time audio hooks
(c-declare  #<<end-of-c-declare

#include <math.h>

void rtaudio_register(void (*)(int), void (*)(float), void (*)(float*,float*));

double srate=0;
float buffer;

void my_realtime_init(int samplerate) { srate=(double)samplerate; buffer=0; }
void my_realtime_input(float v) { }
void my_realtime_output(float *v1,float *v2) {
  static double t=0;
  buffer = 0.95*sin(2*M_PI*440.*t);
  *v1=*v2=(float)buffer;
  t+=1/srate;
}

end-of-c-declare
)
(c-initialize "rtaudio_register(my_realtime_init,my_realtime_input,my_realtime_output);")

;; Generate a tone using the beep utility
(define tone-end #f)
(define (generate-tone parent widget event x y)
  ; Make sure neither frequency or duration were left blank
  (if (= (string-length (glgui-widget-get parent frequency-field 'label)) 0) (set-frequency 1))
  (if (= (string-length (glgui-widget-get parent duration-field 'label)) 0) (glgui-widget-set! parent duration-field 'label "1 ms"))
  (rtaudio-start 8000 0.5)
  (set! tone-end (+ (current-milliseconds) (string->number (chop-units (glgui-widget-get parent duration-field 'label))))))

;;  (shell-command (string-append "beep -f " (chop-units (glgui-widget-get parent frequency-field 'label))
;;                                " -l " (chop-units (glgui-widget-get parent duration-field 'label)))))

;; Logarithmic scale for frequency (so middle A [440] falls about in the middle)
;; Adapted from https://stackoverflow.com/questions/846221/logarithmic-slider

(define min-freq (log *min-frequency*))
(define max-freq (log *max-frequency*))
(define frequency-scale (/ (- max-freq min-freq) (- *max-position* *min-position*)))
;; Convert slider position to frequency
(define (position->frequency position)
  (inexact->exact (round (exp (+ min-freq (* frequency-scale (- position *min-position*)))))))
;; Convert frequency to slider position
(define (frequency->position freq) (/ (- (log freq) min-freq) (+ frequency-scale *min-position*)))
;; Link slider to text field display of frequency
(define (adjust-frequency)
  (glgui-widget-set! gui frequency-field 'label (string-append (number->string
    (position->frequency (glgui-widget-get gui slider 'value))) " Hz")))
;; Set frequency slider and display
(define (set-frequency freq)
  (glgui-widget-set! gui slider 'value (frequency->position freq))
  (glgui-widget-set! gui frequency-field 'label (string-append (number->string freq) " Hz")))
;; Buttons increase and decrease frequency by one octave
(define (adjust-octave modifier)
  (let ((new-freq (* (string->number (chop-units (glgui-widget-get gui frequency-field 'label))) modifier)))
    (if (and (>= new-freq *min-frequency*) (<= new-freq *max-frequency*)) (set-frequency new-freq))))
(define (decrease-octave parent widget event x y) (adjust-octave 0.5))
(define (increase-octave parent widget event x y) (adjust-octave 2))

;; User input

;; Chop off units (Hz and ms) from value
(define (chop-units text)
  (let* ((text-length (string-length text))
         ; Prevent negative position in string if text-length shorter than units
         (text-pos (if (< (- text-length 3) 0) 0 (- text-length 3)))
         (text-units (substring text text-pos text-length)))
    (if (or (equal? text-units " Hz") (equal? text-units " ms")) (substring text 0 text-pos) text)))
(define (chop-units! parent widget event x y)
  (glgui-widget-set! parent widget 'label (chop-units (glgui-widget-get parent widget 'label))))
;; Only allow numbers within range of min-value and max-value
(define (num-only min-value max-value old-value)
  (lambda (parent widget)
    (let* ((current-value (chop-units (glgui-widget-get parent widget 'label)))
           (current-numified (string->number current-value)))
      (if (or (= (string-length current-value) 0) ; Allow field to be empty
              (and current-numified (>= current-numified min-value) (<= current-numified max-value)))
          (set! old-value current-value)
          (glgui-widget-set! parent widget 'label old-value)))))

(main
;; initialization
  (lambda (w h)
    (make-window 540 360)
    (glgui-orientation-set! GUI_LANDSCAPE)
    (set! gui (make-glgui))

    ;; Background color
    (let ((w (glgui-width-get))
          (h (glgui-height-get)))
      (glgui-box gui 0 0 w h *background-color*))

    ;; Frequency slider
    (set! slider (glgui-slider gui 20 280 500 60 *min-position* *max-position* #f White *foreground-color* *accent-color* #f ascii_18.fnt ascii_18.fnt #f White))
    (glgui-widget-set! gui slider 'showlabels #f)
    (glgui-widget-set! gui slider 'value (frequency->position 440))
    (glgui-widget-set! gui slider 'callback (lambda (parent widget event x y) (adjust-frequency)))

    ;; Frequency display
    (set! frequency-field (glgui-inputlabel gui 210 230 120 30 "440 Hz" ascii_18.fnt *text-color* *foreground-color*))
    (glgui-widget-set! gui frequency-field 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui frequency-field 'onfocuscb chop-units!)
    (set! frequency-range (num-only *min-frequency* *max-frequency* (glgui-widget-get gui frequency-field 'label)))
    (glgui-widget-set! gui frequency-field 'aftercharcb (lambda (parent widget event x y)
      (frequency-range parent widget)
      (let ((freq (string->number (glgui-widget-get parent widget 'label))))
        (if freq (glgui-widget-set! parent slider 'value (frequency->position freq))))))

     ;; Octave buttons
    (set! lower-button (glgui-button-string gui 140 230 50 30 "<" ascii_18.fnt decrease-octave))
    (set! higher-button (glgui-button-string gui 350 230 50 30 ">" ascii_18.fnt increase-octave))

    ;; Play button
    (set! play-button (glgui-button-string gui 230 125 80 50 "Play" ascii_25.fnt generate-tone))

    ;; Style buttons
    (for-each (lambda (button)
                (glgui-widget-set! gui button 'button-normal-color *foreground-color*)
                (glgui-widget-set! gui button 'button-selected-color *accent-color*)
                (glgui-widget-set! gui button 'solid-color #t)
                (glgui-widget-set! gui button 'rounded #f))
              (list lower-button higher-button play-button))

    ;; General Controls
    (glgui-label gui 20 40 80 30 "Duration" ascii_18.fnt *foreground-color*)
    (set! duration-field (glgui-inputlabel gui 110 40 120 30 "200 ms" ascii_18.fnt *text-color* *foreground-color*))
    (glgui-widget-set! gui duration-field 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui duration-field 'onfocuscb chop-units!)
    (set! duration-range (num-only 1 600000 (glgui-widget-get gui duration-field 'label)))
    (glgui-widget-set! gui duration-field 'aftercharcb (lambda (parent widget event x y) (frequency-range parent widget)))
    (glgui-label gui 410 40 60 30 "Note" ascii_18.fnt *foreground-color*)
    (set! note (glgui-dropdownbox gui 470 40 50 30
      (map (lambda (str)
        (lambda (lg lw x y w h s) (if s (glgui:draw-box x y w h *foreground-color*))
          (glgui:draw-text-left (+ x 5) y (- w 10) h str ascii_18.fnt *text-color*)))
        (list "A" "B" "C" "D" "E" "F" "G"))
      *accent-color* *foreground-color* *accent-color*))
    (glgui-widget-set! gui note 'scrollcolor *accent-color*)
    (glgui-widget-set! gui note 'callback (lambda (parent widget event x y)
      (set-frequency (table-ref notes (glgui-widget-get parent widget 'current)))))

  )
;; events
  (lambda (t x y)
    (if (= t EVENT_KEYPRESS) (begin
      (if (= x EVENT_KEYESCAPE) (terminate))))
    ;; Also update frequency when dragging slider (callback is only on release)
    (if (and (glgui-widget-get gui slider 'downval) (= t EVENT_MOTION)) (adjust-frequency))
    (cond [(and tone-end (>= (current-milliseconds) tone-end))
      (rtaudio-stop)
      (set! tone-end #f)])
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend) (terminate))
;; resume
  (lambda () (glgui-resume))
)

;; eof
