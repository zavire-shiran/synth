#lang racket

(require racket)
(require racket/generator)

(define (sine-func phase)
  (sin (* 2 phase pi)))

(define (square-func phase)
  (if (> phase 0.5) 1.0 -1.0))

(define (triangle-func phase)
  (cond ((< phase 0.25) (* phase 4))
        ((< phase 0.75) (- 1 (* 4 (- phase 0.25))))
        (else (* 4 (- phase 1)))))

(define (oscillator sample-rate frequency-signal func)
  (generator ()
             (let loop ((phase 0.0))
               (when (> phase 1.0) (loop (- phase 1.0)))
               (yield (func phase))
               (loop (+ phase (/ (frequency-signal) sample-rate))))))

(define (constant-signal value)
  (generator () (let loop ()
                  (yield value)
                  (loop))))

(define (s32-file filename signal)
  (with-output-to-file filename
    (thunk
     (for ((s (in-producer signal (void))))
       (display (integer->integer-bytes s 4 #t))))
    #:mode 'binary
    #:exists 'truncate))

(define (signal-sample-take-num num signal)
  (generator ()
             (for ((i (in-range num)))
               (yield (signal)))))

(define (float-signal-to-integer-signal gain signal)
  (generator ()
             (for ((s (in-producer signal)))
               (yield (exact-round (* gain s))))))

(module+ main #f
         (let* ((osc (oscillator 44100 (constant-signal 440) triangle-func))
                (s1 (float-signal-to-integer-signal (expt 2 28) osc)))
           (s32-file "music.s32" (signal-sample-take-num 44100 s1))))
