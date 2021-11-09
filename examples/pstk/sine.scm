; Port of Allegro saw wave example from C to Chicken Scheme
; https://github.com/liballeg/allegro5/blob/master/examples/ex_saw.c

; apt install liballegro5-dev
; chicken-install -sudo allegro

(import (prefix allegro "al:"))
(import (chicken memory))
(import (chicken bitwise))

(define *samples-per-buffer* 1024)
(define *stream-frequency* 44100)
(define +pi+ 3.141592)

; Initialize Allegro and audio addon
(unless (al:init) (print "Could not initialize Allegro."))
(unless (al:audio-addon-install) (print "Could not initialize sound."))
(al:reserve-samples 0)

(define stream (al:make-audio-stream 8 *samples-per-buffer* *stream-frequency* 'float32 'one))
(unless stream (print "Could not create stream."))

(unless (al:audio-stream-attach-to-mixer! stream (al:default-mixer))
  (print "Could not attach stream to mixer."))

(define queue (al:make-event-queue))
(al:event-queue-register-source! queue (al:audio-stream-event-source stream))

(let ((event (al:make-event)))
  (let main-loop ((n 200))
    ; Grab and handle events
    (when (and (> n 0) (al:event-queue-wait! queue event)
      (case (al:event-type event) ('audio-stream-fragment
        (let ((buf (al:audio-stream-fragment stream)))
          ; If the stream is not ready for new data, buf will be null.
          (if (not buf) (main-loop n) (begin
            (let ((adr (pointer->address buf)))
              (let loop ((i 0)
                         (frequency 440))
                (when (< i *samples-per-buffer*)
                  ; al:audio-stream-fragment returns a C pointer. Use (chicken
                  ; memory) module to operate on foreign pointer objects.
                  (begin (print n ":" i ":adr:" (+ adr (* i 32)) ":" (sin (* 2 +pi+ frequency (/ (+ (* *samples-per-buffer* n) i) *stream-frequency*))))
                         (pointer-f32-set! (address->pointer (+ adr (* i 32)))
                           (sin (* 2 +pi+ frequency (/ (+ (* *samples-per-buffer* n) i) *stream-frequency*))))
                           (print "read back: " (pointer-f32-ref (address->pointer (+ adr (* i 32)))))
                         (loop (+ i 1) 440))))
              (unless (al:audio-stream-fragment-set! stream buf)
                (print "Error setting stream fragment")))
            ; Repeat
            (main-loop (- n 1)))))))))))

(al:audio-stream-drain stream)
;(al:free-event-queue! queue); Declared as finalizer by make-event-queue
;(al:free-audio-stream! stream) ; Declared as finalizer by make-audio-stream
;(al:audio-addon-uninstall) ; Caused error: corrupted size vs. prev_size
