#lang racket/base

(require racket/list
         racket/match
         racket/vector
         plot)

;(define path "/home/turon/Research/ChemistrySet/reports")
(define path "/Users/turon/rep/code/ChemistrySet/reports")
(define pldi-path (string-append path "/pldi-final"))
(define out-path "/Users/turon/rep/papers/pldi-2012-reagents/graphs/")

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
  (cons (map car sorted) data))

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

#;(define ((plot3d-series x-label y-label z-label) series-name series-data)
  (plot3d (surface3d (surface-fn series-data) 
                     1 7.9999 0 (- (vector-length series-data) 1)
                     #:color '(100 220 120) 
                     #:line-color '(100 100 100))
          #:angle 45 #:altitude 23
          #:title series-name
          #:x-label x-label
          #:y-label y-label
          #:z-label z-label))

#;(define (plot3d-all data x-label y-label z-label)
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
       #:alpha 0.4 #:width 4
       #:color (vector-ref colors (modulo i (vector-length colors))))))
  (plot (apply mix param-lines)
        #:title series-name
        #:x-label x-label
        #:y-label y-label))

(define-struct series (name display-name color style))

(define (plot2d-collected run-set set-name series-list multiplier)
  (define work-vec (list->vector (car run-set)))
  (define data (cdr run-set))
  (define size 165)
  (define pseries-list (map (lambda (series-data) (apply make-series series-data)) series-list))  
  (for/list ([work-index (in-range 0 (vector-length work-vec))])
    (define (make-line series)
      (define series-data (hash-ref data (series-name series))) 
      (lines
       (for/list ([j (in-range 0 (vector-length (vector-ref series-data work-index)))])
         (vector (+ 1 j) (* multiplier (vector-ref (vector-ref series-data work-index) j))))
       #:width 2
       #:color (series-color series)
       #:style (series-style series)
       ;#:alpha 0.6
       #:label #f))
       ;#:label (series-display-name series)))
    (define (make-legend-line series)
      (define series-data (hash-ref data (series-name series))) 
      (lines (list #(3 3))
             #:width 3
             #:color (series-color series)
             #:style (series-style series)
             ;#:alpha 0.6
             #:label (series-display-name series)))
    (cons
      (parameterize ([plot-font-family 'swiss]
                     [plot-tick-size 6] )
        (plot (map make-line pseries-list)
              #:out-file (string-append out-path set-name "-" (number->string (vector-ref work-vec work-index)) ".pdf")
              #:title (string-append
                       set-name ": "
                       (if (< (vector-ref work-vec work-index) 500) "high" "low")
                       " contention")
                       ;(number->string (* 0.25 (/ (vector-ref work-vec work-index) 100)))
                       ;"μs work")
              #:x-label #f ;"Threads"
              #:y-label #f ;"Throughput"
              #:width size
              #:height size
              #:legend-anchor 'bottom))
      (parameterize ([plot-font-family 'swiss]
                     [plot-tick-size 0] 
                     [x-axis-ticks? #f]
                     [y-axis-ticks? #f])
        (plot (map make-legend-line pseries-list)
              #:out-file (string-append out-path set-name "-" (number->string (vector-ref work-vec work-index)) "-legend.pdf")
              #:title #f
              #:x-min 1
              #:x-max 8
              #:y-min 0
              #:y-max 8
              #:x-label #f 
              #:y-label #f 
              #:width size
              #:height size
              #:legend-anchor 'center)))))
   

(define (load type path bench-name date)
  (define files (file-list type path bench-name date))
  (load-param-benchmark files))

;(load-and-plot3d path "PushPop" "latest")
;(load-and-plot3d path "PushPop" "2011.08.20.18.00.24")
;(load-and-plot3d path "IncDec" "2011.08.05.10.15.32")

;(define data-current (load path "PushPop" "latest"))
;(define data-full (load path "PushPop" "2011.08.20.18.00.24"))
;(define data-new-elim (load path "PushPop" "2011.08.23.12.10.26"))

;(define data-rtp-full (load "rtp" path "PushPop" "2011.08.23.22.57.02"))
;(define data-tp-full (load "tp" path "PushPop" "2011.08.23.22.57.02"))
;(define normalized (normalize-to-first data-rtp-full))

; 129.10.115.127

;(define data-pushpop (load "rtp" (string-append path "/PushPopElim") "PushPop" "latest"))
;(plot3d-all normalized "Threads" "Work/50" "Speedup")
;(plot3d-all data-tp-full "Threads" "Work/50" "Op throughput")

;(plot3d-all (normalize-to-first data-pushpop) "Threads" "Work" "Speedup")
;(plot3d-all (normalize-to-first data-enqdeq) "Threads" "Work" "Speedup")

#;(define (plot3d-compare data s1 s2)
  ((plot3d-series "Threads" "Work/50" "Throughput ratio")
   (string-append s1 " versus " s2)
   (combine-serii (lambda (x y) (/ y x)) 
                  (hash-ref data s1)
                  (hash-ref data s2))))

(define (plot2d-compare data s1 s2)
  ((plot2d-series "Threads" "Throughput ratio")
   (string-append s1 " versus " s2)
   (combine-serii (lambda (x y) (/ y x)) 
                  (hash-ref data s1)
                  (hash-ref data s2))))

;(plot3d-compare data-pushpop "rElim" "handElim")
;(plot3d-compare data-ppe "rElim" "handElim")

;(plot3d-compare data-pushpop "rElim" "hand")
;(plot3d-compare data-ppe "rElim" "hand")

;(plot3d-compare data-pushpop "rElim" "rTreiber")
;(plot3d-compare data-ppe "rElim" "rTreiber")

;(plot3d-compare data-pushpop "rTreiber" "hand")
;(plot3d-compare data-ppe "rTreiber" "hand")

;(hash-map (normalize-to-first data-pushpop) (plot2d-series "Threads" "Speedup"))
;(hash-map data-pushpop (plot2d-series "Threads" "Throughput"))
;(hash-map data-enqdeq (plot2d-series "Threads" "Throughput"))

(define data-pushpop (load "rtp" pldi-path "PushPop" "latest"))
(define data-stacktran (load "rtp" pldi-path "StackTransfer" "latest"))
(define data-enqdeq (load "rtp" pldi-path "EnqDeq" "latest"))
(define data-queuetran (load "rtp" pldi-path "QueueTransfer" "latest"))

(plot2d-collected 
 data-pushpop 
 "PushPop" 
 '(("rTreiber" "Reagent-Treiber" 1 0)
   ("hand" "Hand-Treiber" 2 1)
   ("lock" "Lock-based" 3 2)
   ("stm" "STM-based" 4 4)
   #;("rElim" "Reagent-Elim" 5 4))
 2)
 ;#(100 1000))
 ;#(30 40 50 100 300 500 3000))

(plot2d-collected 
 data-stacktran
 "StackTransfer" 
 '(("reagent" "Reagent-Treiber" 1 0)
   ("lock" "Lock-based" 3 2)
   ("stm" "STM-based" 4 4)
   #;("rElim" "Reagent-Elim" 5 4))
 3)
 ;#(100 1000))
 ;#(10 30 40 50 100 300 500 3000 5000))

;(plot2d-collected data-enqdeq "EnqDeq" #(50 500 5000))

(plot2d-collected 
 data-enqdeq
 "EnqDeq" 
 '(("reagent" "Reagent-based" 1 0)
   ("hand" "Hand-build" 2 1)
   ("lock" "Lock-based" 3 2)
   ("stm" "STM-based" 4 4)
   #;("simpleR" "Reagent-Simple" 5 4))
 2)
 ;#(100 1000))
 ;#(50 500 5000))

(plot2d-collected 
 data-queuetran
 "QueueTransfer" 
 '(("reagent" "Reagent-MSQ" 1 0)
   ("lock" "Lock-based" 3 2)
   ("stm" "STM-based" 4 4)
   #;("simple" "Reagent-Simple" 5 4))
 3)
 ;#(100 1000))
 ;#(40 100 1000))