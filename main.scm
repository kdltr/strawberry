(import scheme chicken srfi-1)
(use (prefix sdl2 sdl2:) posix)

(set-signal-handler! signal/int exit)
(set-signal-handler! signal/term exit)

(define graphics-file (car (command-line-arguments)))

(define ww 800)
(define wh 480)

(sdl2:set-hint! 'render-scale-quality "0")
(sdl2:set-main-ready!)
(sdl2:init! '(video))
(on-exit sdl2:quit!)

(define window (sdl2:create-window! "Confiture de fraises"
                                    0 0 800 480))
(define render (sdl2:create-renderer! window -1 '(accelerated)))
(set! (sdl2:render-logical-size render) (list ww wh))


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

(let loop ()
  (reload-graphics)
  (let ((t (sdl2:get-ticks)))
    (set! dt (- t now))
    (set! now t))
  (unless dirty
    (safe (show-frame)))
  (sdl2:render-clear! render)
  (sdl2:render-present! render)
  (loop))

