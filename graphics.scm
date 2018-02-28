(void)

(define-record track dt data)
(define-record channel vol env osc)

(define white (sdl2:make-color 255 255 255))
(define black (sdl2:make-color 0 0 0))
(define red (sdl2:make-color 255 0 0))

(define (noteon? ev)
  (eq? (cadr ev) 'noteon))

(define (track-ended? tr)
  (null? (track-data tr)))

(define *midi-time* 0)
(define *last-player-event* -inf.0)

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


(define rect (sdl2:make-rect 0 0 10 10))
(define *scale* 10)
(define *player-next-note* 0)

(define (blink-on-note)
  (let ((data (track-data input-track)))
    (unless (null? data)
      (let* ((evt (car data))
             (evt-dt (car evt))
             (evt-type (cadr evt))
             (evt-note (caddr evt)))
        (cond ((>= *midi-time* evt-dt)
               (set! *scale* 10)
               (set! *last-player-event* evt-dt)
               (set! *player-next-note* evt-note)
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
        (cond ((= diff 0)
               (print "PERFECT"))
              ((> tolerence-good diff)
               (print "GOOD"))
              ((> tolerence-bad diff)
               (print "BAD"))
              (else (print "MISSED")))))))


(define (handle-event ev)
  (when (eq? (sdl2:event-type ev) 'key-down)
    (if (eq? (sdl2:keyboard-event-scancode ev)
             'escape)
        (exit)
        (register-input)))
  (void)
  )

(define (show-frame)
  (for-each advance-track all-tracks all-channels)
  (advance-track chords-track
                 (vector-ref chord-channels
                             (min 2 *next-chord-channel*))
                 #t)
  (blink-on-note)

  (set! (sdl2:render-draw-color render) white)
  (sdl2:render-clear! render)

  (set! (sdl2:render-draw-color render) red)
  (set! (sdl2:render-scale render) (list *scale* *scale*))
  (sdl2:render-fill-rect! render rect)
  (set! (sdl2:render-scale render) (list 1 1))

  (set! *scale* (max 0 (- *scale* (* dt 5))))

  (let* ((avail (pa:stream-write-available))
         (len (min avail 512)))
    (when (> len 512) (print len))
    (unless (zero? len)
      (fill-buf! dspbuf len)
      (pa:write-stream! dspbuf len)))
  (set! *midi-time* (+ *midi-time* dt))
  (when (every track-ended? all-tracks*)
    (exit))
)