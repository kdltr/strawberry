(use trace srfi-4 miscmacros bitstring srfi-18 tcp)

(define (log . args)
  (apply fprintf (current-error-port) args))

(define log void)

;; BEGIN TEST

(define us (expt 10 6))

(define ticks/qnote 0)
(define us/qnote 0)
(define s/tick 0)

(define in-track 0)

(define (noteon note vel)
  (fprintf out "list ~A ~A;" note vel)
  (flush-output out))

(define (noteoff note vel)
  (fprintf out "list ~A 0;" note)
  (flush-output out))

(define tracks '())

(define current-name #f)
(define current-track '())
(define current-delta #f)

(define (reg-track)
  (unless (null? current-track)
    (push! (cons current-name (reverse current-track)) tracks)
    (set! current-name #f)
    (set! current-track '())))

(define (reg-name name)
  (set! current-name name))

(define (reg-delta dt)
  (set! current-delta dt))

(define (noteon note vel)
  (push! (list current-delta 'noteon note vel)
         current-track))

(define (noteoff note vel)
  (push! (list current-delta 'noteoff note vel)
         current-track))



#;(trace reg-track reg-name reg-delta noteon noteoff)
;; END TEST


(define data
  (with-input-from-file "test.mid"
    (lambda ()
      (read-u8vector))))

;; Variable length integers encoding
(define (read-vli bs)
  (let loop ((rest bs) (acc 0))
    (bitmatch rest
      (((0 1)
        (num 7)
        (rest bitstring))
       (values (+ (* (expt 2 7) acc) num)
               rest))
      (((1 1)
        (num 7)
        (rest bitstring))
       (loop rest (+ (* (expt 2 7) acc) num)))
      (else
        (log "Malformed variable-length integer")))))


(define (read-header data)
  (bitmatch data
    ((("MThd")
      (6 32)
      (format 16)
      (tracks 16)
      (0 1)
      (division-spec 15)
      (rest bitstring))
     (log "MIDI format: ~A. ~A track(s), ~A ticks per quarter-note~%"
          format tracks division-spec)
     (set! ticks/qnote division-spec)
     (read-track 3 rest))
    (((""))
     'ok)
    (else
      (log "Malformed or unsupported MIDI file"))))

(define (read-track n data)
  (unless (zero? n)
    (bitmatch data
      ((("MTrk")
        (length 32)
        (data (* length 8) bitstring)
        (rest bitstring))
       (log "- Track of length ~A~%" length)
       (reg-track)
       (read-track-data data)
       (read-track (sub1 n) rest))
      (((type 32 bitstring)
        (length 32)
        (data (* length 8))
        (rest bitstring))
       (log "Warning: unknown chunk \"~A\" of length ~A~%"
            (bitstring->string type) length)
       (read-track n rest))
      (((""))
       'ok)
      (else
        (log "Malformed MIDI file")))))

(define (read-track-data bs #!optional (status 0) (channel 0))
  (unless (zero? (bitstring-length bs))
    (receive (delta rest)
      (read-vli bs)
      (log "  - Delta ~A: " (* delta s/tick in-track))
      (reg-delta (* delta s/tick in-track))
      #;(thread-sleep! (* delta s/tick in-track))
      (read-event rest status channel))))

(define (read-event bs status channel)
  (bitmatch bs
    ;; Sysex events
    (((#xF0 8)
      (rest bitstring))
     (read-track-data (read-sysex-event "F0" rest)))
    (((#xF7 8)
      (rest bitstring))
     (read-track-data (read-sysex-event "F7" rest)))
    
    ;; Meta events
    (((#xFF 8)
      (type 8)
      (rest bitstring))
     (read-track-data (read-meta-event type rest)))
    
    ;; MIDI events
    (((1 1)
      (rest bitstring))
     (bitmatch bs
       (((status 4)
         (channel 4)
         (rest bitstring))
        (read-track-data (read-midi-data status channel rest)
                         status channel))))
    
    (((0 1)
      (rest bitstring))
     (read-track-data (read-midi-data status channel bs)
                      status channel))

    (((other bitstring))
     (log "Unknown data: ~A~%"
          (bitstring->blob other)))
     ))

(define (read-sysex-event type bs)
  (receive (length rest)
    (read-vli bs)
    (bitmatch rest
      (((data (* 8 length) bitstring)
        (rest bitstring))
       (log "~X Sysex event of length ~A: ~A~%"
            type length (bitstring->blob data))
       rest))))

(define (read-meta-event type bs)
  (receive (length rest)
     (read-vli bs)
    (bitmatch rest
      (((data (* 8 length) bitstring)
        (rest bitstring))
       (case type
         ((#x00)
          (bitmatch data
            (((sequence 16))
             (log "Sequence Number ~A~%" sequence))))
         ((#x01)
          (log "Text Event \"~A\"~%" (bitstring->string data)))
         ((#x02)
          (log "Copyright Notice \"~A\"~%" (bitstring->string data)))
         ((#x03)
          (log "Sequence/Track Name \"~A\"~%" (bitstring->string data))
          (reg-name (bitstring->string data)))
         ((#x04)
          (log "Instrument Name \"~A\"~%" (bitstring->string data)))
         ((#x05)
          (log "Lyric \"~A\"~%" (bitstring->string data)))
         ((#x06)
          (log "Marker \"~A\"~%" (bitstring->string data)))
         ((#x07)
          (log "Cue Point \"~A\"~%" (bitstring->string data)))
         ((#x20)
          (bitmatch data
            (((channel 8))
             (log "MIDI Channel Prefix ~A~%" channel))))
         ((#x2F)
          (log "End of Track~%")
          (set! in-track 1))
         ((#x51)
          (bitmatch data
            (((tempo 24))
             (log "Set Tempo ~A us/quarter-note~%" tempo)
             (set! us/qnote tempo)
             (set! s/tick (/ (/ us/qnote ticks/qnote) us))
             )))
         ((#x54)
          (bitmatch data
            (((hours 8)
              (minutes 8)
              (seconds 8)
              (frac 8))
             (log "SMTPE Offset ~A:~A:~A.~A~%"
                  hours minutes seconds frac))))
         ((#x58)
          (bitmatch data
            (((numerator 8)
              (denominator 8)
              (clocks 8)
              (notes 8))
             (log "Time Signature ~A/~A, ~A clocks per tick, ~A 32th notes per 24 clocks~%"
                  numerator (expt 2 denominator) clocks notes))))
         ((#x59)
          (bitmatch data
            (((sharps 8)
              (major-minor 8))
             (log "Key Signature ~A ~A, ~A key~%"
                  (abs sharps) (if (< sharps 0) "flats" "sharps") (if (zero? major-minor) "major" "minor")))))
         ((#x7F)
          (log "Sequencer-Specific Meta Event ~A~%"
               (bitstring->blob data)))
         (else
           (log "Meta event type ~X of length ~A: ~A~%"
                type length (if (member type '(3 6))
                                (bitstring->string data)
                                (bitstring->blob data)))))
       rest
       ))))

(define (read-midi-data status channel bs)
  (case status
    ((#xC #xD)
     (bitmatch bs
       (((data 8)
         (rest bitstring))
        (case status
          ((#xC)
           (log "Program Change on channel ~A: ~X~%"
                channel data))
          ((#xD)
           (log "Channel Key Pressure on channel ~A: ~X~%"
                channel data)))
        rest)))
    (else
      (bitmatch bs
        (((data1 8)
          (data2 8)
          (rest bitstring))
         (case status
           ((#x8)
            (log "Note off on channel ~A: ~X ~X~%"
                 channel data1 data2)
            (noteoff data1 data2))
           ((#x9)
            (log "Note ON on channel ~A: ~X ~X~%"
                 channel data1 data2)
            (noteon data1 data2))
           ((#xA)
            (log "Aftertouch on channel ~A: ~X ~X~%"
                 channel data1 data2))
           ((#xB)
            (log "Controler Change on channel ~A: ~X ~X~%"
                 channel data1 data2))
           ((#xE)
            (log "Pitch Bend on channel ~A: ~X ~X~%"
                 channel data1 data2)))
         rest)))))

(read-header data)
(reg-track) ;; register last track
(pp tracks)