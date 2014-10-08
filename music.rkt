#lang racket

(require racket)
(require (only-in srfi/1 zip take-while drop-while))

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

(define (multiply-signals . signals)
  (lambda()
    (for/product ((signal signals)) (signal))))

(define (gain-signal gain signal)
  (lambda ()
    (* gain (signal))))

(define (s32-file filename signal)
  (call-with-output-file filename
    (lambda (out)
     (for ((s (in-producer signal (void))))
       (display (integer->integer-bytes s 4 #t) out)))
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

(define (note->pitch note octave)
  (+ (* octave 12)
     (case (note)
       ((b# c) 0)
       ((c# db) 1)
       ((d) 2)
       ((eb d#) 3)
       ((e fb) 4)
       ((e# f) 5)
       ((f# gb) 6)
       ((g) 7)
       ((g# ab) 8)
       ((a) 9)
       ((a# bb) 10)
       ((b cb) 11))))

(define (instrument sample-rate signal-gen notes)
  (set! notes (sort notes < #:key first))
  (let ((playing-notes (list))
        (sample-num 0))
    (displayln "Making an instrument")
    (lambda ()
      (let* ((time (/ sample-num sample-rate))
             (new-notes (take-while (lambda (n) (<= (first n) time)) notes)))
        (when (not (null? new-notes)) (displayln (list "New notes:" new-notes)))
        (set! notes (drop-while (lambda (n) (<= (first n) time)) notes))
        (for ((note new-notes))
          (set! playing-notes
                (cons (list (second note) (apply signal-gen (cons sample-rate note)))
                      playing-notes)))
        (set! playing-notes (drop-while (lambda (n) (< (first n) time))
                                        (sort playing-notes < #:key first)))
        (set! sample-num (+ sample-num 1))
        (if (or (not (null? notes))
                (not (null? playing-notes)))
            (for/sum ((note playing-notes))
              (let ((sample ((second note))))
                (if (void? sample) 0 sample)))
            (void))))))

(module+
    main #f
    (require profile)

    (define (fm-instr sample-rate start end pitch)
      (displayln (list sample-rate start end pitch))
      (let* ((freq (pitch->frequency pitch))
             (osc (gain-signal (/ freq 4) (oscillator sample-rate (constant-signal (/ freq 2)) sine-func)))
             (osc2 (oscillator sample-rate (add-signals osc (constant-signal freq)) sine-func)))
        osc2))

    (displayln "Starting mow")
    (s32-file "music.s32" (float-signal-to-integer-signal
                           (expt 2 28)
                           (instrument 44100 fm-instr
                                       '((0 0.5 60)
                                         (0.5 1.0 62)
                                         (1.0 1.5 64)
                                         (1.5 2.0 65)
                                         (2.0 2.5 67)
                                         (2.5 3.0 69)
                                         (3.0 3.5 71)
                                         (3.5 4.0 72)))))
    (displayln "Finished mow"))
;    (profile-thunk
;     (thunk
;      (s32-file "envelope.s32" (float-signal-to-integer-signal (expt 2 28) (segment-signal 44100 '(0 0) '(0.1 1) '(0.9 1) '(1.0 0))))
;      (let* ((osc (gain-signal 30 (oscillator 44100 (constant-signal 10) sine-func)))
;             (osc2 (gain-signal 100 (oscillator 44100 (add-signals osc (constant-signal 55)) sine-func)))
;             (osc3 (oscillator 44100 (add-signals osc2 (constant-signal 220)) sine-func))
;             (s1 (float-signal-to-integer-signal (expt 2 28) osc3)))
;        (s32-file "music.s32" (signal-sample-take-num 44100 s1))))))
