#lang racket

(require racket)
(require racket/generator)
(require (only-in srfi/1 zip))

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

(define (add-signals . signals)
  (generator () (let loop ((accum 0))
                  (for ((signal signals))
                    (set! accum (+ accum (signal))))
                  (yield accum)
                  (loop 0))))

(define (gain-signal gain signal)
  (generator () (for ((s (in-producer signal (void))))
                  (yield s))))

; TODO: make a buffer to encode into
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
             (for ((s (in-producer signal (void))))
               (yield (exact-round (* gain s))))))

(define (append-signals . signals)
  (generator ()
             (for* ((signal signals)
                    (s (in-producer signal (void))))
               (yield s))))

(define (segment-signal sample-rate . points)
  (let ((segment
         (lambda (length start end)
           (generator ()
                      (for ((i (in-range length)))
                        (yield (+ start (* (/ i length) (- end start)))))))))
    (apply append-signals
           (for/list ((start points)
                      (end (cdr points)))
             (match-let (((list start-time start-value) start)
                         ((list end-time end-value) end))
               (segment (* sample-rate (- end-time start-time)) start-value end-value))))))

(module+ main #f
         (s32-file "envelope.s32" (float-signal-to-integer-signal (expt 2 28) (segment-signal 44100 '(0 0) '(0.1 1) '(0.9 1) '(1.0 0)))))
;         (let* ((osc (gain-signal 10 (oscillator 44100 (constant-signal 10) sine-func)))
;                (osc2 (gain-signal 100 (oscillator 44100 (add-signals osc (constant-signal 55)) sine-func)))
;                (osc3 (oscillator 44100 (add-signals osc2 (constant-signal 220)) sine-func))
;                (s1 (float-signal-to-integer-signal (expt 2 28) osc3)))
;           (s32-file "music.s32" (signal-sample-take-num 44100 s1))))
