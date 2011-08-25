#lang racket/base

(require racket/list
         racket/match
         racket/vector
         "../plt-stuff/plot/plot3d.rkt"
         "../plt-stuff/plot/plot2d.rkt")

(define (normalize-to-first data)
  (define (normalize-vector vec)
    (vector-map (lambda (x) (/ x (vector-ref vec 0))) vec))
  (for/hash ([(series-name series-data) (in-hash data)])
    (values series-name (vector-map normalize-vector series-data))))

; Parses a simple CSV file with series name in first column
; returns (List (Pair String Vector))
(define (load-file name)
  (call-with-input-file name         
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match-define (cons series values) (regexp-split #rx"," line))
        (cons series (list->vector (map string->number values)))))))

(define (load-param-benchmark runs) 
  (define data (make-hash))
  (define sorted (sort runs (lambda (r1 r2) (< (car r1) (car r2)))))
  (define run-count (length runs))
  (for ([run sorted]
        [i (in-range 0 run-count)])
    (match-define (cons param fname) run)
    (for ([series-data (load-file fname)])
      (match-define (cons series values) series-data)
      (define runs-for-series 
        (hash-ref! data series (lambda () (make-vector run-count))))
      (vector-set! runs-for-series i values)))
  data)

(define (file-list type path bench-name date)
  (define (parse-file-name file-name)
    (match file-name
      [(regexp (regexp (string-append "^" type "\\." bench-name "-(.*)\\." date)) (list _ work))
       (string->number work)]
      [_ #f]))
  (for*/list ([file-path (directory-list path)]
              [file-name (in-value (path->string file-path))]
              [parsed    (in-value (parse-file-name file-name))] #:when parsed)
    (cons parsed (string-append path "/" file-name))))

(define ((surface-fn series-data) x y)
  (define xf (inexact->exact (floor (- x 1))))
  (define yf (inexact->exact (floor y)))
  (define perc-x (- x (floor x)))
  ;(define perc-y (- x (floor x)))
  (define z0 (vector-ref (vector-ref series-data yf) xf))
  (define z1 (vector-ref (vector-ref series-data yf) (+ 1 xf)))  
  (+ (* perc-x z1) (* (- 1 perc-x) z0)))

(define ((plot3d-series x-label y-label z-label) series-name series-data)
  (parameterize ((plot3d-font-family 'swiss))
    (plot3d (surface3d (surface-fn series-data) 
                       1 7.9999 0 (- (vector-length series-data) 1)
                       #:color '(100 220 120) 
                       #:line-color '(100 100 100))
            #:angle 45 #:altitude 23
            #:title series-name
            #:x-label x-label
            #:y-label y-label
            #:z-label z-label)))

(define (plot3d-all data x-label y-label z-label)
  (hash-map data (plot3d-series x-label y-label z-label)))

(define (combine-serii op data1 data2)
  (for/vector ([i (in-range 0 (min (vector-length data1)
                                   (vector-length data2)))])
    (for/vector ([j (in-range 0 (min (vector-length (vector-ref data1 i))
                                     (vector-length (vector-ref data2 i))))])
      (op (vector-ref (vector-ref data1 i) j)
          (vector-ref (vector-ref data2 i) j)))))

(define ((plot2d-series x-label y-label) series-name series-data)
  (define colors 
    (vector 'red 
            'orange 
            'yellow 
            'green
            'blue 
            'indigo 
            'violet))
  (define param-lines
    (for/list ([i (in-range 0 (vector-length series-data))])
      (lines
       (for/list ([j (in-range 0 (vector-length (vector-ref series-data i)))])
         (vector (+ 1 j) (vector-ref (vector-ref series-data i) j)))
       1 8 0 8 #:alpha 0.4 #:width 4
       #:color (vector-ref colors (modulo i (vector-length colors))))))
  (plot2d (apply mix param-lines)
          #:title series-name
          #:x-label x-label
          #:y-label y-label))

(define (load type path bench-name date)
  (define files (file-list type path bench-name date))
  (load-param-benchmark files))

;(define path "/home/turon/ChemistrySet/reports")
(define path "/Users/turon/ChemistrySet/reports")

;(load-and-plot3d path "PushPop" "latest")
;(load-and-plot3d path "PushPop" "2011.08.20.18.00.24")
;(load-and-plot3d path "IncDec" "2011.08.05.10.15.32")

;(define data-current (load path "PushPop" "latest"))
;(define data-full (load path "PushPop" "2011.08.20.18.00.24"))
;(define data-new-elim (load path "PushPop" "2011.08.23.12.10.26"))

(define data-rtp-full (load "rtp" path "PushPop" "2011.08.23.22.57.02"))
(define data-tp-full (load "tp" path "PushPop" "2011.08.23.22.57.02"))
(define normalized (normalize-to-first data-rtp-full))

(plot3d-all normalized "Threads" "Work/50" "Speedup")
(plot3d-all data-tp-full "Threads" "Work/50" "Op throughput")

((plot3d-series "Threads" "Work/50" "Op throughput ratio")
 "Reagent-Elim versus Hand-Elim" 
 (combine-serii (lambda (x y) (/ x y)) 
                (hash-ref data-tp-full "r-elim")
                (hash-ref data-tp-full "handElim")))

((plot3d-series "Threads" "Work/50" "Speedup difference")
 "Reagent-Elim versus Hand-Elim (normalized)"
 (combine-serii (lambda (x y) (- x y)) 
                (hash-ref normalized "r-elim")
                (hash-ref normalized "handElim")))

((plot3d-series "Threads" "Work/50" "Op throughput ratio")
 "Reagent-Elim versus Hand"               
 (combine-serii (lambda (x y) (/ x y)) 
                (hash-ref data-tp-full "r-elim")
                (hash-ref data-tp-full "hand")))

((plot3d-series "Threads" "Work/50" "Speedup difference")
 "Reagent-Elim versus Hand (normalized)"               
 (combine-serii (lambda (x y) (- x y)) 
                (hash-ref normalized "r-elim")
                (hash-ref normalized "hand")))

((plot3d-series "Threads" "Work/50" "Speedup difference")
 "HandElim versus Hand (normalized)"               
 (combine-serii (lambda (x y) (- x y)) 
                (hash-ref normalized "handElim")
                (hash-ref normalized "hand")))

(hash-map normalized (plot2d-series "Threads" "Speedup"))
(hash-map data-tp-full (plot2d-series "Threads" "Op throughput"))
;(hash-map (normalize-to-first data-new-elim) plot2d-series)