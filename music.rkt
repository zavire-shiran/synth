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
  (let ((phase 0.0))
    (lambda ()
      (when (> phase 1.0) (set! phase (- phase 1.0)))
      (begin0
          (func phase)
        (set! phase (+ phase (/ (frequency-signal) sample-rate)))))))

(define (constant-signal value)
  (lambda () value))

(define (add-signals . signals)
  (lambda ()
    (for/sum ((signal signals)) (signal))))

(define (gain-signal gain signal)
  (lambda ()
    (* gain (signal))))

;TODO: make large buffer for output
(define (s32-file filename signal)
  (with-output-to-file filename
    (thunk
     (for ((s (in-producer signal (void))))
       (display (integer->integer-bytes s 4 #t))))
     #:mode 'binary
     #:exists 'truncate))

(define (signal-sample-take-num num signal)
  (let ((n num))
    (lambda ()
      (if (> n 0)
          (begin
            (set! n (- n 1))
            (signal))
          (void)))))

(define (float-signal-to-integer-signal gain signal)
  (lambda ()
    (let ((s (signal)))
      (if (void? s)
          (void)
          (exact-round (* gain s))))))

(define (segment-signal sample-rate . points)
  (let ((i 0))
    (define (iter)
      (if (eq? (cdr points) null)
          (void)
          (let ((time (/ i sample-rate))
                (startt (first (first points)))
                (startv (second (first points)))
                (endt (first (second points)))
                (endv (second (second points))))
            (if (>= time endt)
                (begin (set! points (cdr points))
                       (iter))
                (begin0
                    (+ startv
                       (* (- endv startv)
                          (/ (- time startt)
                             (- endt startt))))
                  (set! i (+ i 1)))))))
    iter))
            
(define (pitch->frequency pitch)
  (* 440 (expt 2 (/ (- pitch 69.0) 12.0))))

(module+ main #f
         (s32-file "envelope.s32" (float-signal-to-integer-signal (expt 2 28) (segment-signal 44100 '(0 0) '(0.1 1) '(0.9 1) '(1.0 0)))))
;         (require profile)
;         (profile-thunk
;          (thunk
;           (let* ((osc (gain-signal 10 (oscillator 44100 (constant-signal 10) sine-func)))
;                  (osc2 (gain-signal 100 (oscillator 44100 (add-signals osc (constant-signal 55)) sine-func)))
;                  (osc3 (oscillator 44100 (add-signals osc2 (constant-signal 220)) sine-func))
;                  (s1 (float-signal-to-integer-signal (expt 2 28) osc3)))
;             (s32-file "music.s32" (signal-sample-take-num 44100 s1))))))
