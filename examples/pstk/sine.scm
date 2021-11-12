; apt install liballegro5-dev
; chicken-install -sudo allegro

(import (prefix allegro "al:"))
(import (chicken memory))

(define +pi+ 3.141592)

; Initialize Allegro and audio addon
(unless (al:init) (print "Could not initialize Allegro."))
(unless (al:audio-addon-install) (print "Could not initialize sound."))
(al:reserve-samples 0)

(define (generate-tone frequency duration)
  (let* ((samples-per-buffer 1024)
         (stream-frequency 44100)
         (amplitude 0.5)
         (stream (al:make-audio-stream 8 samples-per-buffer stream-frequency 'float32 'one))
         (queue (al:make-event-queue))
         (event (al:make-event)))

    (unless (al:audio-stream-attach-to-mixer! stream (al:default-mixer))
      (print "Could not attach stream to mixer."))
    (al:event-queue-register-source! queue (al:audio-stream-event-source stream))

    (let event-loop ((n 0))
      ; Grab and handle events
      (when (and (< n (/ (* (/ duration 1000) stream-frequency) samples-per-buffer))
                 (al:event-queue-wait! queue event))
        (case (al:event-type event) ('audio-stream-fragment
          (let ((buffer (al:audio-stream-fragment stream)))
            ; If the stream is not ready for new data, buffer will be null.
            (if (not buffer) (event-loop n) (begin
              (let ((adr (pointer->address buffer)))
                (let loop ((i 0))
                  (when (< i samples-per-buffer)
                    (let ((time (/ (+ (* samples-per-buffer n) i) stream-frequency)))
                      ; al:audio-stream-fragment returns a C pointer. Use (chicken
                      ; memory) module to operate on foreign pointer objects.
                      ; Iterate over array four bytes at a time since 32-bit depth.
                      (pointer-f32-set! (address->pointer (+ adr (* i 4)))
                        (* amplitude (sin (* 2 +pi+ frequency time))))
                      (loop (+ i 1)))))
                (unless (al:audio-stream-fragment-set! stream buffer)
                  (print "Error setting stream fragment")))
              ; Repeat
              (event-loop (+ n 1)))))))))

    (al:audio-stream-drain stream)))
