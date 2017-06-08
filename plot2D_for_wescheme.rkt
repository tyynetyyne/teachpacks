;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname testing_wescheme_small_bug_error) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require racket/list)
;(provide plot2D
;         func)
;         dots)

;; this is needed in WeScheme (comment out in DrRacket)
(define (reverse-flatten-into x lst)
  (if (not (number? x))
      (foldl reverse-flatten-into lst x)
      (cons x lst)))

;(define (flatten lst)
;  (reverse (reverse-flatten-into lst '())))

;; constants
(define SIZE 400)
(define STEPS 100)
(define BG (empty-scene SIZE SIZE))
(define LABEL-BOX (rectangle 110 20 180 "transparent"))
(define AXIS-HEIGHT 10)
(define AXIS (rectangle SIZE AXIS-HEIGHT "solid" "transparent"))
(define SPACE (square 10 "solid" "transparent"))
(define FONT-SIZE 20)
(define FONT-SIZE-AXIS 15)
(define AXIS-WIDTH 60)
(define FUNC 1)
(define DOTS 2)
(define DOT-SIZE 3)

(define (convert i)
  (cond [(= i 1) "blue"]
        [(= i 2) "red"]
        [(= i 3) "green"]
        [(= i 4) "orange"]
        [else "black"]))

;;-----------------------------------------------------------------------------
;; magnitude-pos : Number Number -> Number
(define (magnitude-pos x i)
  (if (< x 10)
      (* (floor x) (expt 10 i))
      (magnitude-pos (/ x 10) (add1 i))))

;; magnitude-neg : Number Number -> Number
(define (magnitude-neg x i)
  (if (> x -10)
      (* (ceiling x) (expt 10 i))
      (magnitude-neg (/ x 10) (add1 i))))

;; magnitude-pos-dec : Number Number -> Number
(define (magnitude-pos-dec x i)
  (if (> x 1)
      (* (floor x) (expt 10 (- i)))
      (magnitude-pos-dec (* x 10) (add1 i))))

;; magnitude-neg-dec : Number Number -> Number
(define (magnitude-neg-dec x i)
  (if (< x -1)
      (* (ceiling x) (expt 10 (- i)))
      (magnitude-neg-dec (* x 10) (add1 i))))

;; magnitude-rounding : Number -> Number
(define (magnitude-rounding x)
  (cond [(< 0 x 1)(magnitude-pos-dec x 0)]
        [(<= 1 x)(magnitude-pos x 0)]
        [(> 0 x -1)(magnitude-neg-dec x 0)]
        [(<= x -1)(magnitude-neg x 0)]
        [else #false])) 

;; if both max and min are on the positive side of zero
;; start-pos : Number Number -> Number
(define (start-pos min step i)
  (if (> (* i step) min)
      (* i step)
      (start-pos min step (add1 i))))

;; if both max and min are on the negative side of zero
;; start-neg : Number Number -> Number
(define (start-neg max step i)
  (if (< (* i (- step)) max)
      (* i (- step))
      (start-neg max step (add1 i))))

;; axix-points : Number Number Number -> List<Number>
(define (axis-points min max step)
  (cond [(= min 0)
         (range min max step)]
        [(= max 0)
         (reverse (range max min (- step)))]
        [(< min max 0)
         (reverse (range (start-neg max step 0) min (- step)))]
        [(> max min 0)
         (range (start-pos min step 0) max step)]
        [(< min 0 max)
         (remove 0 (append (reverse (range 0 min (- step)))(range 0 max step)))]
        [else #false])) 

;; axis-step : Number Number -> Number
(define (axis-step min max)
  (magnitude-rounding (/ (- max min) 5)))

;; axis : Number Number -> List<Number>
(define (axis min max)
  (axis-points min max (axis-step min max)))

;; label : String -> Image
(define (label t)
  (text (number->string (exact->inexact t)) FONT-SIZE-AXIS "black"))

;; shift-axis : Number -> Number
(define (shift-axis x)
    (+ x (/ FONT-SIZE-AXIS 2)))

;; plot-x-axis : List<Number> -> Image
(define (plot-x-axis xs x-step x-min)
  (foldl place-image (rectangle (+ SIZE FONT-SIZE-AXIS) (+ FONT-SIZE-AXIS 5) 0 "transparent")
         (map label xs) (map shift-axis 
                             (map (convert-x x-step x-min) xs))
                             (make-list (length xs)(+ 5 (/ FONT-SIZE-AXIS 2) ))))  

;; plot-y-axis : List<Number> -> Image
(define (plot-y-axis ys y-step y-min)
  (foldl place-image (rectangle AXIS-WIDTH (+ SIZE FONT-SIZE-AXIS) 0 "transparent")
         (map label ys)  (make-list (length ys) (/ AXIS-WIDTH 2)) (map (convert-y y-step y-min) ys)))  

;; -------------------------------------------------------------------
;; func : Function Number Number String -> Function
(define (func f start end label)
  (lambda (l-color)
    (let* [(step (/ (- end start) STEPS))
           (x (range start (+ end step) step))
           (y (map f x))
           (linecolor (convert l-color))]           
        (cons FUNC (cons label (cons linecolor (map make-posn x y)))))))

;; dots : Number Number String -> Function
(define (dots posns start-x end-x start-y end-y label)
  (lambda (l-color)
    (let* [(linecolor (convert l-color))]           
        (cons DOTS (cons label (cons linecolor (cons (make-posn start-x start-y)
                                                    (cons (make-posn end-x end-y)
                                                          posns))))))))
                                                
;; convert-y : Number Number -> Number
(define (convert-y y-step y-min)
  (lambda (y)
    (- SIZE (* y-step (- y y-min)))))

;; convert-x : Number Number -> Number
(define (convert-x x-step x-min)
  (lambda (x)
    (* x-step (- x x-min))))

;; plot : List<Posn> Image Color -> Image
(define (plot posn-list target linecolor x-step y-step x-min x-max y-min y-max)
  (if (or (empty? posn-list)
          (< (length posn-list) 2))
      target
      (plot (rest posn-list)
            (add-line target
                      ((convert-x x-step x-min) (posn-x (first posn-list)))
                      ((convert-y y-step y-min) (posn-y (first posn-list)))
                      ((convert-x x-step x-min) (posn-x (second posn-list)))
                      ((convert-y y-step y-min) (posn-y (second posn-list)))
                      linecolor)
            linecolor x-step y-step x-min x-max y-min y-max)))

;; plot-dots : List<Posn> Image Color -> Image
(define (plot-dots posn-list target linecolor x-step y-step x-min x-max y-min y-max)
  (if (or (empty? posn-list)
          (< (length posn-list) 1))
      target
      (plot-dots (rest posn-list)
            (place-image (circle DOT-SIZE "outline" linecolor) 
                         ((convert-x x-step x-min) (posn-x (first posn-list)))
                         ((convert-y y-step y-min) (posn-y (first posn-list)))
                         target)
            linecolor x-step y-step x-min x-max y-min y-max)))

;; plot-color :
(define (plot-with-color x-step y-step x-min x-max y-min y-max)
  (lambda (posn-list target)
    (if (= (first posn-list) FUNC)
        (plot (rest (rest (rest posn-list))) 
              target 
              (third posn-list) 
              x-step 
              y-step 
              x-min 
              x-max 
              y-min 
              y-max)
        (plot-dots (rest (rest (rest (rest (rest posn-list)))))
              target 
              (third posn-list) 
              x-step 
              y-step 
              x-min 
              x-max 
              y-min 
              y-max))))

;; plot2D-help : List<Posn> String String String -> Image
(define (plot2D-help list-of-func x-min x-max y-min y-max)
  (let [(x-step (/ SIZE (- x-max x-min)))
        (y-step (/ SIZE (- y-max y-min)))]
    (if (list? list-of-func)
        (foldl (plot-with-color x-step y-step x-min x-max y-min y-max) BG list-of-func)
        ((plot-with-color x-step y-step x-min x-max y-min y-max) list-of-func BG))))

;; plot-label : List -> Image
(define (plot-label func img)
  (if (= (first func)FUNC)
      (above (overlay/xy (rectangle 35 1.5 "solid" (third func))
                         -70 -9
                         (overlay/xy (text (second func) 13 "black")
                                     -4 -1
                                     LABEL-BOX))
             img)
      (above (overlay/xy (circle DOT-SIZE "outline" (third func))
                         -70 -6
                         (overlay/xy (text (second func) 13 "black")
                                     -4 -1
                                     LABEL-BOX))
             img)))

;; plot-labels : List -> Image
(define (plot-labels list-of-func)
  (if (list? list-of-func)
      (foldl plot-label empty-image list-of-func)
      (plot-label empty-image list-of-func)))

(define (plot2D-axis list-of-func x-min x-max y-min y-max)
  (let* [(x-step (/ SIZE (- x-max x-min)))
         (y-step (/ SIZE (- y-max y-min)))
         (plotted (add-axes (plot2D-help list-of-func x-min x-max y-min y-max) x-step y-step x-min y-min))        
         (x-values (axis x-min x-max))
         (y-values (axis y-min y-max))
         (y-axis-marks (create-y-axis y-values y-step y-min))
         (x-axis-marks (create-x-axis x-values x-step x-min))
         (marked-plot (above x-axis-marks
                             (beside y-axis-marks plotted y-axis-marks)
                             x-axis-marks))]
     (overlay/xy (plot-y-axis y-values y-step y-min)
                 AXIS-WIDTH (- AXIS-HEIGHT)
                 (above marked-plot
                        (plot-x-axis x-values x-step x-min)))))

(define (add-mark-y y-step y-min)
  (lambda(y img)
    (add-line img
              0 
              ((convert-y y-step y-min) y)
              10
              ((convert-y y-step y-min) y)
              "black")))

(define (add-mark-x x-step x-min)
  (lambda(x img)
    (add-line img
              ((convert-x x-step x-min) x)
              0
              ((convert-x x-step x-min) x)
              10
              "black")))

(define (create-x-axis xs x-step x-min)
  (foldl (add-mark-x x-step x-min) AXIS xs))

(define (create-y-axis ys y-step y-min)
  (foldl (add-mark-y y-step y-min) (rotate 90 AXIS) ys))

(define (add-axes plots x-step y-step x-min y-min)
  (scene+line (scene+line plots
                      ((convert-x x-step x-min) 0)
                      0
                      ((convert-x x-step x-min) 0)
                      SIZE
                      "black")
            0 
            ((convert-y y-step y-min) 0)
            SIZE
            ((convert-y y-step y-min) 0)
            "black")) 

;; add-line-colors : List -> List
(define (add-line-colors f-list ready i)
  (if (empty? f-list)
      ready
      (if (not (list? f-list))
          (list (f-list i))
          (add-line-colors (rest f-list)(cons ((first f-list) i) ready) (add1 i))))) 

(define (get-x-list func)
  (map posn-x (rest (rest (rest func)))))

(define (find-max-x list-of-func)
   (if (not (list? list-of-func))
            (apply max (get-x-list list-of-func))
            (apply max (flatten (map get-x-list list-of-func)))))

(define (find-min-x list-of-func)
   (if (not (list? list-of-func))
            (apply min (get-x-list list-of-func))
            (apply min (flatten (map get-x-list list-of-func)))))

(define (get-y-list func)
  (map posn-y (rest (rest (rest func)))))

(define (find-max-y list-of-func)
   (if (not (list? list-of-func))
            (apply max (get-y-list list-of-func))
            (apply max (flatten (map get-y-list list-of-func)))))

(define (find-min-y list-of-func)
   (if (not (list? list-of-func))
            (apply min (get-y-list list-of-func))
            (apply min (flatten (map get-y-list list-of-func)))))

(check-expect (find-max-x (list (cons "label" (cons 3 (list (make-posn 2 3)
                                                            (make-posn 4 5)
                                                            (make-posn 10 0))))))
              10)

(check-expect (find-max-x (list (cons "label" (cons 3 (list (make-posn 2 3)
                                                            (make-posn 4 5)
                                                            (make-posn 10 0))))
                                (cons "label" (cons 3 (list (make-posn 2 3)
                                                            (make-posn 40 5)
                                                            (make-posn 10 0))))))
              40)

(define (plot2D list-of-f x-label y-label title)
  (let* [(list-of-func (add-line-colors list-of-f '() 1))
         (min-x (find-min-x list-of-func))
         (min-y (find-min-y list-of-func))
         (max-x (find-max-x list-of-func))
         (max-y (find-max-y list-of-func))
         (plotted-labels (plot-labels list-of-func))
         (plots (overlay/xy (frame plotted-labels) -75 -15
                            (plot2D-axis list-of-func min-x max-x min-y max-y)))
         (plots-f (overlay plots
                               (rectangle (+ (image-width plots)(* 8 AXIS-HEIGHT))
                                          (+ (image-height plots)(* 8 AXIS-HEIGHT))
                                          "solid"
                                          "transparent")))
         (y-offset (/ FONT-SIZE-AXIS 2))
         (x-offset (/ (- (image-width plots) SIZE (* 2 AXIS-HEIGHT)) 2))
         (y-text (rotate 90 (text y-label FONT-SIZE "black")))
         (x-text (text x-label FONT-SIZE "black"))
         (title (text title FONT-SIZE "black"))
         (x-middle (/ (image-width plots-f) 2))
         (y-middle (/ (image-height plots-f) 2))]
    (place-image y-text
                 (* 2 AXIS-HEIGHT)
                 (- y-middle y-offset)
                 (place-image x-text
                              (+ x-middle x-offset) 
                              (- (image-height plots-f) (* 2 AXIS-HEIGHT)) 
                              (place-image title 
                                           (+ x-middle x-offset) 
                                           (* 2 AXIS-HEIGHT)
                                           plots-f)))))          
           
;;--------------------------------------------------------------------
;; testing
(define (plot1 x)
  (+ (* -2 x) -3))

(define (plot2 x)
  (+ (* -2 x x) 7))

(define (plot3 x)
  (+ (* -2 x x x) 7))

(define p1 (list (make-posn 0 0)(make-posn 100 200)(make-posn 200 300)(make-posn 300 400)))
;(plot2D (dots p1 0 10 0 10 "p1") "x" "y" "tekstiä")

(plot2D (func plot1 -0.1 0.1 "y=x^2+3") "x" "y" "this is working")  ; OK

;(plot2D (func plot1 -100 100 "y=x^2+3") "x" "y" "this is not working")  ; NOT OK

;(plot2D (list (func plot1 -100 100 "y=x^2+3") (func plot2 -100 100 "y=-2x^2+7")
;             (dots p1 0 10 0 10 "p1") ) "x" "y" "title1")

;(plot2D (func sin (- pi) pi "sin(x)") "x" "y" "title2")
