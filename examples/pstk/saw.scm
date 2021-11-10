; Port of Allegro saw wave example from C to Chicken Scheme
; https://github.com/liballeg/allegro5/blob/master/examples/ex_saw.c

(import (prefix allegro "al:"))
(import (chicken memory))
(import (chicken bitwise))

(define *samples-per-buffer* 1024)

; Initialize Allegro and audio addon
(unless (al:init) (print "Could not initialize Allegro."))
(unless (al:audio-addon-install) (print "Could not initialize sound."))
(al:reserve-samples 0)

(define stream (al:make-audio-stream 8 *samples-per-buffer* 22050 'uint8 'one))
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
                         (val 0)
                         (pitch #x10000))
                (when (< i *samples-per-buffer*)
                  ; al:audio-stream-fragment returns a C pointer. Use (chicken
                  ; memory) module to operate on foreign pointer objects.
                  ; Iterate over array one byte at a time since 8-bit depth.
                  (begin (pointer-u8-set! (address->pointer (+ adr i))
                           (bitwise-and (arithmetic-shift val -16) 255))
                         (loop (+ i 1) (+ val pitch) (+ pitch 1)))))
              (unless (al:audio-stream-fragment-set! stream buf)
                (print "Error setting stream fragment")))
            ; Repeat
            (main-loop (- n 1)))))))))))

(al:audio-stream-drain stream)
(al:audio-addon-uninstall)
