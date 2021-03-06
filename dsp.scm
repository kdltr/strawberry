(define (modf x) (- x (truncate x)))

(define *base* 440)
(define (midi->freq n)
  (* (/ *base* 32)
     (expt 2 (/ (- n 9) 12))))

(define (make-basic-osc func)
  (lambda (freq #!optional (phase 0))
    (lambda args
      (cond ((null? args)
             (begin0 (if (zero? freq) 0 (func phase))
                     (set! phase
                       (+ phase (/ (* 2pi freq)
                                   *sample-rate*)))
                     (when (>= phase 2pi)
                       (set! phase (- phase 2pi)))
                     ))
            ((eq? (car args) 'freq)
             (set! freq (cadr args)))
            ((eq? (car args) 'phase)
             (set! phase (cadr args)))
            (else #f)
            ))))

(define (pulse phase width)
  (if (< phase (* width 2pi)) 1 -1))

(define (pulse~ freq width)
  (let* ((func (lambda (phase) (pulse phase width)))
         (osc ((make-basic-osc func) freq)))
    (lambda args
      (cond ((apply osc args) => identity)
            ((eq? (car args) 'width)
             (set! width (cadr args)))
            (else #f)))))

(define (saw phase)
  (- 1 (* (/ 1 pi) phase)))

(define (triangle phase)
  (if (< phase pi)
      (+ -1 (* (/ 2 pi) phase))
      (- 3 (* (/ 2 pi) phase))))

(define sin~ (make-basic-osc sin))
#;(define pulse~ (make-basic-osc pulse))
(define saw~ (make-basic-osc saw))
(define triangle~ (make-basic-osc triangle))

(define mask15bits (bitwise-xor #x7fff (expt 2 14)))
(define mask7bits (bitwise-xor #x7fff (expt 2 6)))

(define (white~ 7bit div)
  (let ((reg (if 7bit #x40 #x4000))
        (t 0))
    (define (shift-reg!)
      (let* ((bit1 (bitwise-and reg #x0001))
             (bit2 (bitwise-and (arithmetic-shift reg -1) #x0001))
             (new-bit (bitwise-xor bit1 bit2))
             (new-reg-15 (bitwise-ior (bitwise-and (arithmetic-shift reg -1)
                                                   mask15bits)
                                      (arithmetic-shift new-bit 14)))
             (new-reg (if 7bit
                          (bitwise-ior (bitwise-and new-reg-15 mask7bits)
                                       (arithmetic-shift new-bit 6))
                          new-reg-15)))
        (set! reg new-reg)))
    (lambda args
      (cond ((null? args)
             (begin0 (if (zero? (bitwise-and reg #x0001)) 1 -1)
                     (set! t (+ t (/ 1 *sample-rate*)))
                     (when (>= t div)
                       (shift-reg!)
                       (set! t 0))))
            ((eq? (car args) 'freq)
             (set! div (if (> (cadr args) 69)
                             1/20000
                             1/512)))
            ((eq? (car args) 'div)
             (set! div (cadr args)))
            ((eq? (car args) '7bit)
             (set! 7bit (cadr args))
             (set! reg #x40))))))

(define (ramp~ dur)
  (let ((cur 0))
    (lambda args
      (cond ((null? args)
             (begin0 (if (< cur dur) (/ cur dur) 1)
                     (set! cur (+ cur (/ 1 *sample-rate*)))))
            ((eq? (car args) 'reset)
             (set! cur 0))))))

(define (envelope~ attack release)
  (let ((t 0)
        (up? #t)
        (up (ramp~ attack))
        (down (ramp~ release)))
    (lambda args
      (cond ((null? args)
             (begin0
               #;(if up? (up) (+ 1 (* -1 (down))))
               (if (< t attack)
                   (up)
                   (+ 1 (* -1 (down))))
               (set! t (+ t (/ 1 *sample-rate*)))))
            ((eq? (car args) 'reset)
             (set! t 0)
             (set! up? #t)
             (up 'reset)
             (down 'reset))
            ((eq? (car args) 'release)
             (set! up? #f))))))

(define (fill-buf! b l)
  (do ((i 0 (+ i 1)))
      ((>= i l))
      (let ((val (dac~)))
        (f32vector-set! b i val)
        )))

(define +~ +)
(define *~ *)
