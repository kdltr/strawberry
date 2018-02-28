(void)

(define-record track dt data)
(define-record channel vol env osc)

(define white (sdl2:make-color 255 255 255))
(define black (sdl2:make-color 0 0 0))
(define red (sdl2:make-color 255 0 0))
(define blue (sdl2:make-color 0 0 255))

(define (ld fn)
  (let ((surf (img:load* fn)))
    (begin0
      (sdl2:create-texture-from-surface render surf)
      (sdl2:free-surface! surf))))

(define %bg (ld "bg.png"))
(define %oizo-normal (ld "oizo_normal.png"))
(define %oizo-pi (ld "oizo_pi.png"))
(define %oizo-happy (ld "oizo_happy.png"))
(define %oizo-meh (ld "oizo_meh.png"))
(define %oizo-angry (ld "oizo_angry.png"))
(define %you-normal (ld "you_normal.png"))
(define %you-up (ld "you_up.png"))
(define %you-down (ld "you_down.png"))
(define %you-happy (ld "you_happy.png"))
(define %you-sad (ld "you_sad.png"))

(define (noteon? ev)
  (eq? (cadr ev) 'noteon))

(define (track-ended? tr)
  (null? (track-data tr)))

(define *anim-time* 0)
(define tempo (/ us/qnote 4000000))
(define *midi-time* 0)
(define *oizo-frame* %oizo-normal)
(define *you-frame* %you-normal)
(define *last-player-event* -inf.0)

(print "Tempo: " tempo)
(printf "MIDI tracks: ~S~%" (map car midi))

(define input-track
  (make-track 0 (filter noteon? (alist-ref "input" midi equal?))))
(define input-channel
  (make-channel 0 (envelope~ 0 0) (sin~ 0)))

(print (track-data input-track))

(define chords-track
  (make-track 0 (alist-ref "chords" midi equal?)))
(define *next-chord-channel* 0)
(define chord-channels (make-vector 3))
(do ((i 0 (add1 i)))
    ((= i 3))
  (vector-set! chord-channels i
               (make-channel 0
                             (envelope~ 0.01 1)
                             (sin~ 0))))

(define lead-track
  (make-track 0 (alist-ref "lead" midi equal?)))
(define lead-channel (make-channel 0
                                   (envelope~ 0.01 0.25)
                                   (triangle~ 0 0.5)))

(define bass-track
  (make-track 0 (alist-ref "bass" midi equal?)))
(define bass-channel (make-channel 0
                                   (envelope~ 0.01 1)
                                   (triangle~ 0)))

(define perc-track
  (make-track 0 (alist-ref "perc" midi equal?)))
(define perc-channel (make-channel 0
                                   (envelope~ 0.01 0.1)
                                   (white~ #f 1/8000)))

(define all-tracks
  (list lead-track bass-track perc-track))
(define all-tracks* (cons chords-track all-tracks))

(define all-channels
  (list lead-channel bass-channel perc-channel))
(define all-channels*
  (append all-channels
          (list (vector-ref chord-channels 0)
                (vector-ref chord-channels 1)
                (vector-ref chord-channels 2))))

(define (advance-track track channel #!optional (chord? #f))
  (let ((data (track-data track)))
    (unless (null? data)
      (let* ((next-evt (car data))
             (next-evt-dt (car next-evt))
             (next-evt-type (cadr next-evt))
             (next-evt-note (caddr next-evt))
             (next-evt-velocity (cadddr next-evt)))
        (if (>= *midi-time* next-evt-dt)
            (begin
              (cond ((eq? next-evt-type 'noteon)
                     ((channel-env channel) 'reset)
                     ((channel-osc channel)
                      'freq (midi->freq next-evt-note))
                     (channel-vol-set! channel
                       (/ next-evt-velocity 127))
                     (when (eq? track lead-track)
                       (set! *oizo-frame* %oizo-pi)
                       (set! *anim-time* tempo))
                     (when chord?
                       (set! *next-chord-channel*
                         (add1 (min 1 *next-chord-channel*)))))
                    ((eq? next-evt-type 'noteoff)
                     #;(channel-vol-set! channel 0)
                     ((channel-env channel) 'release)
                     (when chord?
                       (set! *next-chord-channel*
                         (sub1 (max 1 *next-chord-channel*))))
                       ))
              (track-data-set! track (cdr data))
              (unless chord? (advance-track track channel)))
            )))))

(define dspbuf (make-f32vector 512 0 #t #t))

(define (dac~)
  (fold
        (lambda (c sample)
          (+ sample
             (* 0.5
                (channel-vol c)
                ((channel-env c))
                ((channel-osc c)))))
        0
        all-channels*))

(define (blink-on-note)
  (let ((data (track-data input-track)))
    (unless (null? data)
      (let* ((evt (car data))
             (evt-dt (car evt))
             (evt-type (cadr evt))
             (evt-note (caddr evt)))
        (cond ((>= *midi-time* evt-dt)
               (set! *last-player-event* evt-dt)
               (track-data-set! input-track (cdr data))
               (blink-on-note)) )))))

;; TODO tweak that
(define tolerence-good 0.1)
(define tolerence-bad 0.250)

(define (register-input)
  (let ((data (track-data input-track)))
    (unless (null? data)
      (let* ((next-event (car data))
             (next-dt (car next-event))
             (next-type (cadr next-event))
             (diff-next (- next-dt *midi-time*))
             (diff-prev (- *midi-time* *last-player-event*))
             (diff (min diff-next diff-prev)))
        (print (list next: diff-next prev: diff-prev diff: diff))
        (set! *anim-time* (* 8 tempo))
        (set! *you-frame*
          (cond ((eq? *you-frame* %you-down)
               %you-up)
              ((eq? *you-frame* %you-up)
               %you-down)
              (else %you-up)))
        (cond ((> tolerence-good diff)
               (set! *oizo-frame* %oizo-normal)
               (print "GOOD"))
              ((> tolerence-bad diff)
               (set! *oizo-frame* %oizo-meh)
               (print "BAD"))
              (else
                (set! *oizo-frame* %oizo-meh)
                (print "MISSED")
                ))))))


(define (handle-event ev)
  (when (eq? (sdl2:event-type ev) 'key-down)
    (if (eq? (sdl2:keyboard-event-scancode ev)
             'escape)
        (exit)
        (register-input)))
  (void)
  )

(define (show-game-frame)
  (for-each advance-track all-tracks all-channels)
  (advance-track chords-track
                 (vector-ref chord-channels
                             (min 2 *next-chord-channel*))
                 #t)
  (blink-on-note)
  
  (when (<= *anim-time* 0)
    (set! *oizo-frame* %oizo-normal)
    (set! *you-frame* %you-normal))

  (sdl2:render-copy! render %bg)
  (sdl2:render-copy! render *oizo-frame*)
  (sdl2:render-copy! render *you-frame*)

  (let* ((avail (pa:stream-write-available))
         (len (min avail 512)))
    (when (> len 512) (print len))
    (unless (zero? len)
      (fill-buf! dspbuf len)
      (pa:write-stream! dspbuf len)))
  
  (set! *midi-time* (+ *midi-time* dt))
  (set! *anim-time* (- *anim-time* dt))
  
  (when (every track-ended? all-tracks*)
    (set! show-frame show-ending-frame))
)

(define (show-ending-frame)
  (set! (sdl2:render-draw-color render) blue)
  (sdl2:render-clear! render)
  )

(define show-frame show-game-frame)