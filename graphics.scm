(void)

(define dspbuf (make-f32vector 512 0 #t #t))

(load "dsp.so")

(define *note* #f)
(define (dac~)
  (if *note*
      (pseudo-random-real)
      0))

(define pi (acos -1))
(define white (sdl2:make-color 255 255 255))
(define black (sdl2:make-color 0 0 0))
(define red (sdl2:make-color 255 0 0))

(define midi-time -1)
(define track (alist-ref "blerb" midi equal?))

(define rect (sdl2:make-rect 0 0 10 10))
(define *scale* 10)

(define (blink-on-note)
  (let* ((evt (car track))
         (evt-dt (car evt))
         (type (cadr evt)))
    (when (and (>= midi-time evt-dt))
      (when (eq? type 'noteon)
        (set! *scale* 10)
        (set! *note* #t))
      (when (eq? type 'noteoff)
        (set! *note* #f))
      (set! midi-time 0)
      (set! track (cdr track)))))


(define (show-frame)
  (unless (null? track)
    (blink-on-note))
  
  (set! (sdl2:render-draw-color render) white)
  (sdl2:render-clear! render)
  
  (set! (sdl2:render-draw-color render) red)
  (set! (sdl2:render-scale render) (list *scale* *scale*))
  (sdl2:render-fill-rect! render rect)
  
  (set! midi-time (+ midi-time dt))
  (set! *scale* (max 0 (- *scale* (* dt 5))))
  
  (let* ((avail (pa:stream-write-available))
         (len (min avail 512)))
    (unless (zero? len)
      (fill-buf! dspbuf len)
      (pa:write-stream! dspbuf len))
    )
  #;(thread-sleep! 0.25))