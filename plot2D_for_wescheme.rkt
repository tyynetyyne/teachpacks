;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname plot2D_for_wescheme) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

;(provide plot2D
;         func
;         dots)

;; constants
(define SIZE 400)
(define BG (empty-scene SIZE SIZE))
(define LABEL-BOX (rectangle 110 20 180 "white"))
(define AXIS-HIGHT 10)
(define AXIS (rectangle SIZE AXIS-HIGHT "solid" "white"))
(define SPACE (square 10 "solid" "white"))
(define FONT-SIZE 20)
(define FONT-SIZE-AXIS 15)
(define AXIS-WIDTH 60)

;; variables for max and min
(define x-min #f)
(define y-min #f)
(define x-max #f)
(define y-max #f)

;; variables for scaling
;(define x-step #f)
;(define y-step #f)

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
  (text (number->string t) FONT-SIZE-AXIS "black"))

;; plot-x-axis : List<Number> -> Image
(define (plot-x-axis xs x-step)
  (foldl place-image (rectangle SIZE FONT-SIZE-AXIS 0 "transparent")
         (map label xs) (map (convert-x x-step) xs) (make-list (length xs)(/ FONT-SIZE-AXIS 2) )))  

;; plot-y-axis : List<Number> -> Image
(define (plot-y-axis ys y-step)
  (foldl place-image (rectangle AXIS-WIDTH SIZE 0 "transparent")
         (map label ys)  (make-list (length ys)(/ AXIS-WIDTH 2) ) (map (convert-y y-step) ys)))  

;; -------------------------------------------------------------------
;; func : Function Number Number String -> Function
(define (func f start end label)
  (lambda (l-color)
    (let* [(step (/ (- end start) 100))
           (x (range start (+ end step) step))
           (y (map f x))
           (linecolor (convert l-color))
           (new-label (overlay/xy (rectangle 35 1.5 "solid" linecolor)
                                  -70 -9
                                  (overlay/xy (text label 13 "black")
                                              -4 -1
                                              LABEL-BOX)))]           
      (begin 
        (if (not (number? x-min))
            (set! x-min start)
            (set! x-min (min x-min start)))
        (if (not (number? y-min))
            (set! y-min (apply min y))
            (set! y-min (min (apply min y) y-min)))
        (if (not (number? x-max))
            (set! x-max end)
            (set! x-max (max x-max end)))
        (if (not (number? y-max))
            (set! y-max (apply max y))
            (set! y-max (max (apply max y) y-max)))
        (cons label (cons linecolor (map make-posn x y)))))))

;; convert-y : Number -> Number
(define (convert-y y-step)
  (lambda (y)
    (- SIZE (* y-step (- y y-min)))))

;; convert-x : Number -> Number
(define (convert-x x-step)
  (lambda (x)
    (* x-step (- x x-min))))

;; plot : List<Posn> Image Color -> Image
(define (plot posn-list target linecolor x-step y-step)
  (if (or (empty? posn-list)
          (< (length posn-list) 2))
      target
      (plot (rest posn-list)
            (add-line target
                      ((convert-x x-step) (posn-x (first posn-list)))
                      ((convert-y y-step) (posn-y (first posn-list)))
                      ((convert-x x-step) (posn-x (second posn-list)))
                      ((convert-y y-step) (posn-y (second posn-list)))
                      linecolor)
            linecolor
            x-step
            y-step)))

;; plot-color :
(define (plot-with-color x-step y-step)
  (lambda (posn-list target)
    (plot (rest (rest posn-list)) target (second posn-list) x-step y-step)))

;; plot2D-help : List<Posn> String String String -> Image
(define (plot2D-help list-of-func)
  (let [(x-step (/ SIZE (- x-max x-min)))
        (y-step (/ SIZE (- y-max y-min)))]
    (if (list? list-of-func)
        (foldl (plot-with-color x-step y-step) BG list-of-func)
        ((plot-with-color x-step y-step) list-of-func BG))))

;; plot-label : List -> Image
(define (plot-label func img)
  (above (overlay/xy (rectangle 35 1.5 "solid" (second func))
                     -70 -9
                     (overlay/xy (text (first func) 13 "black")
                                 -4 -1
                                 LABEL-BOX))
         img))

;; plot-labels : List -> Image
(define (plot-labels list-of-func)
  (if (list? list-of-func)
      (foldl plot-label empty-image list-of-func)
      (plot-label empty-image list-of-func)))

(define (plot2D-axis list-of-func)
  (let* [(x-step (/ SIZE (- x-max x-min)))
         (y-step (/ SIZE (- y-max y-min)))
         (plotted (add-axes (plot2D-help list-of-func) x-step y-step))        
         (x-values (axis x-min x-max))
         (y-values (axis y-min y-max))
         (y-axis-marks (create-y-axis y-values y-step))
         (x-axis-marks (create-x-axis x-values x-step))
         (marked-plot (above x-axis-marks
                             (beside y-axis-marks plotted y-axis-marks)
                             x-axis-marks))]
    (overlay/xy 
     (plot-y-axis y-values y-step)
     AXIS-WIDTH (- AXIS-HIGHT)
     (above marked-plot
            (plot-x-axis x-values x-step)))))

(define (add-mark-y y-step)
  (lambda(y img)
    (add-line img
              0 
              ((convert-y y-step) y)
              10
              ((convert-y y-step) y)
              "black")))

(define (add-mark-x x-step)
  (lambda(x img)
    (add-line img
              ((convert-x x-step) x)
              0
              ((convert-x x-step) x)
              10
              "black")))

(define (create-x-axis xs x-step)
  (foldl (add-mark-x x-step) AXIS xs))

(define (create-y-axis ys y-step)
  (foldl (add-mark-y y-step) (rotate 90 AXIS) ys))

(define (add-axes plots x-step y-step)
  (add-line (add-line plots
                      ((convert-x x-step) 0)
                      0
                      ((convert-x x-step) 0)
                      SIZE
                      "black")
            0 
            ((convert-y y-step) 0)
            SIZE
            ((convert-y y-step) 0)
            "black")) 

;; add-line-colors : List -> List
(define (add-line-colors f-list ready i)
  (if (empty? f-list)
      ready
      (if (not (list? f-list))
          (list (f-list i))
          (add-line-colors (rest f-list)(cons ((first f-list) i) ready) (add1 i))))) 

(define (plot2D list-of-f x-label y-label title)
  (let* [(list-of-func (add-line-colors list-of-f '() 1))
         (plotted-labels (plot-labels list-of-func))
         (plots (overlay/xy (frame plotted-labels) -75 -15
                            (plot2D-axis list-of-func)))]
    (beside (rotate 90 (text y-label FONT-SIZE "black"))
            SPACE
            (above (text title FONT-SIZE "black")
                   SPACE
                   plots       
                   (text x-label FONT-SIZE "black")))))

;;--------------------------------------------------------------------
;; testing
(define (suora x)
  (+ (* 2 x x) 3))

(define (suora2 x)
  (+ (* -2 x x) 7))

(define (suora3 x)
  (+ (* -2 x x x) 7))

(plot2D (func suora -10 10 "y=x^2+3") "x" "y" "otsikko")
(plot2D (list (func suora -1000 1000 "y=x^2+3") (func suora2 -1000 1000 "y=-2x^2+7")
              (func suora3 -100 100 "y=-2x^3+7")) "x" "y" "otsikko")

;(define X (create-x-axis (axis x-min x-max)))
;(define Y (create-y-axis (axis y-min y-max)))
;(plot-y-axis (axis y-min y-max))

