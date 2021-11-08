; apt install liballegro5-dev
; chicken-install -sudo allegro

(import (prefix allegro "al:"))
(import (chicken memory))

; Initialize Allegro and audio addon
(if (not (al:init)) (print "Could not initialize Allegro."))
(if (not (al:audio-addon-install)) (print "Could not initialize sound."))
(al:reserve-samples 0)

; 1024 = SAMPLES_PER_BUFFER
; (make-audio-stream (integer buffer-count) (unsigned-integer samples) (unsigned-integer frequency) audio-depth channel-configuration)
(define stream (al:make-audio-stream 8 1024 22050 'uint8 'one))

(al:audio-stream-attach-to-mixer! stream (al:default-mixer))

(define queue (al:make-event-queue))
(al:event-queue-register-source! queue (al:audio-stream-event-source stream))

(let ([event (al:make-event)])
  (let main-loop ()
    ; Grab and handle events
    (if (al:event-queue-wait! queue event)
        (case (al:event-type event)
          ('audio-stream-fragment
            (let* ((buf (al:audio-stream-fragment stream))
                   ; Need to deal with buf being null
                   (adr (pointer->address buf)))
              (let loop ((i 0))
                (if (< i 1024) ; 1024 = SAMPLES_PER_BUFFER
                  ;(begin (print i)
                  ; buf is a pointer to a C array
                  ; how do we modify C arrays with Chicken
                  ; (pointer-u8-ref buf)
                  ; (pointer->address buf)
                  ;(begin (pointer-u8-set! (address->pointer (+ adr (* i 8))) 42)
                  (begin (print (+ adr (* i 8)))
                         (loop (+ i 1)))))))))

    ; Repeat
    (main-loop)))

(exit)
