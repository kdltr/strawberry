(module portaudio *
(import scheme chicken foreign srfi-4)

(foreign-declare "#include <portaudio.h>")

(define pa-buffer-filler void)

(define-location stream (c-pointer "PaStream") #f)

(define paNoError (foreign-value "paNoError" int))
(define paFloat32 (foreign-value "paFloat32" int))
(define paFramesPerBufferUnspecified
  (foreign-value "paFramesPerBufferUnspecified" int))

;; Low level

(define Pa_Initialize (foreign-lambda int "Pa_Initialize"))
(define Pa_GetErrorText (foreign-lambda c-string "Pa_GetErrorText" int))
(define Pa_Terminate (foreign-lambda int "Pa_Terminate"))
(define Pa_OpenDefaultStream
  (foreign-lambda int "Pa_OpenDefaultStream"
                  (c-pointer (c-pointer "PaStream"))
                  int
                  int
                  unsigned-long
                  double
                  unsigned-long
                  c-pointer
                  c-pointer))
(define Pa_StartStream (foreign-lambda int "Pa_StartStream"
                                       (c-pointer "PaStream")))
(define Pa_StopStream (foreign-lambda int "Pa_StopStream"
                                      (c-pointer "PaStream")))
(define Pa_CloseStream (foreign-lambda int "Pa_CloseStream"
                                       (c-pointer "PaStream")))
(define Pa_WriteStream (foreign-lambda int "Pa_WriteStream"
                                       (c-pointer "PaStream")
                                       nonnull-f32vector
                                       unsigned-long))
(define Pa_GetStreamWriteAvailable (foreign-lambda long "Pa_GetStreamWriteAvailable"
                                                   (c-pointer "PaStream")))


;; High level

(define (check err msg #!optional (terminate #t))
  (if (>= err paNoError)
      err
      (begin
        (error msg (Pa_GetErrorText err))
        (when terminate (Pa_Terminate)))))

(define (init!)
  (check (Pa_Initialize) "could not initialize portaudio" #f))

(define (open-stream! sr)
  (check (Pa_OpenDefaultStream (location stream)
                               0 1 paFloat32 sr
                               256
                               #f #f)
         "could not open stream"))

(define (start-stream!)
  (check (Pa_StartStream stream)
         "could not start stream"))

(define (stop-stream!)
  (check (Pa_StopStream stream)
         "could not stop stream"))

(define (close-stream!)
  (check (Pa_StopStream stream)
         "could not close stream"))

(define (print-stream) (print stream))

(define (stream-write-available)
  (check (Pa_GetStreamWriteAvailable stream)
         "could not get available frames for writing"))

(define (write-stream! buf len)
  (check (Pa_WriteStream stream buf len)
         "could not write stream"))

)