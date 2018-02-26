(import scheme chicken srfi-1 srfi-4)
(use (prefix sdl2 sdl2:) midi posix 
     new-random srfi-4 data-structures
     (prefix portaudio pa:)
     miscmacros)

(set-signal-handler! signal/int exit)
(set-signal-handler! signal/term exit)

(define graphics-file (car (command-line-arguments)))
(define midi (read-midi (cadr (command-line-arguments))))

(define ww 192)
(define wh 256)

(sdl2:set-hint! 'render-scale-quality "0")
(sdl2:set-main-ready!)
(sdl2:init! '(video))

(define window (sdl2:create-window! "Confiture de fraises"
                                    0 0 (* ww 2) (* wh 2)))
(define render (sdl2:create-renderer! window -1 '(accelerated)))
(set! (sdl2:render-logical-size render) (list ww wh))


(pa:init!)
(pa:open-stream! 22050)
(pa:start-stream!)

(load "dsp.so")
(define *sample-rate* 22050)
(define pi (acos -1))
(define 2pi (* 2 pi))

(on-exit (lambda ()
           (print "terminating")
           (sdl2:quit!)
           (pa:close-stream!)
           (pa:Pa_Terminate)))

(define-syntax safe
  (syntax-rules ()
    ((safe body)
     (handle-exceptions exn
       (begin
         (set! dirty #t)
         (print-error-message exn)
         (print-call-chain))
       body
       (set! dirty #f)))))


(define file-mod (file-modification-time graphics-file))
(define now (sdl2:get-ticks))
(define dt 0)
(define dirty #f)

(load graphics-file)

(define (reload-graphics)
  (let ((new-mod (file-modification-time graphics-file)))
    (when (> new-mod file-mod)
      (safe (load graphics-file)))
    (set! file-mod new-mod)))

(define (handle-events)
  (let ((ev (sdl2:poll-event!)))
    (when ev
      (handle-event ev)
      (handle-events))))

;; disable uninteresting events
(for-each (lambda (type) (set! (sdl2:event-state type) #f))
  '(text-editing text-input mouse-motion mouse-button-down
    mouse-button-up mouse-wheel joy-axis-motion joy-ball-motion
    joy-hat-motion joy-button-down joy-button-up joy-device-added
    joy-device-removed controller-axis-motion
    controller-device-added controller-device-removed
    controller-device-remapped finger-down finger-up
    finger-motion dollar-gesture dollar-record
    multi-gesture clipboard-update drop-file))

(let loop ()
  (reload-graphics)
  (let ((t (sdl2:get-ticks)))
    (set! dt (/ (- t now) 1000))
    (set! now t))
  (handle-events)
  (unless dirty
    (safe (show-frame)))
  (sdl2:render-present! render)
  (loop))
