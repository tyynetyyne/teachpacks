#lang racket
(require 2htdp/image)
;(require graphics/graphics) ; make-posn
(require lang/posn) ; make-posn
(require test-engine/racket-tests) ; check-expect

(provide plot2D ;old
         func ;old
         dots ;old
         plot
         function
         points
         labeled-points
         plot-with-axes
         bar-chart
         serie
         lines
         lines-labeled-points
         images)

;; this is needed in WeScheme (comment out in DrRacket)
;(define (reverse-flatten-into x lst)
;  (if (not (number? x))
;      (foldl reverse-flatten-into lst x)
;      (cons x lst)))

;; this is needed in WeScheme (comment out in DrRacket)
;(define (flatten lst)
;  (reverse (reverse-flatten-into lst '())))

;; constants
(define SIZE 400)
(define STEPS 500)
(define BG (empty-scene SIZE SIZE))
(define (label-box x)
         (rectangle (+ (* x 8) 40) 20 180 "white"))
(define AXIS-HEIGHT 10)
(define AXIS (rectangle SIZE AXIS-HEIGHT 0 "transparent"))
(define SPACE (square 10 0 "transparent"))
(define FONT-SIZE 20)
(define FONT-SIZE-AXIS 15)
(define AXIS-WIDTH 60)
(define FUNC 1)
(define DOTS-SOLID-SHOW 2)
(define DOTS-SOLID 3)
(define DOTS-OPEN-SHOW 4)
(define DOTS-OPEN 5)
(define SERIE 6)
(define LINE-DOTS-SOLID-SHOW 7)
(define LINE-DOTS-SOLID 8)
(define LINE-DOTS-OPEN-SHOW 9)
(define LINE-DOTS-OPEN 10)
(define LINE 11)
(define IMAGE 12)
(define DOT-SIZE 3)
(define SERIE-GAP 10)
(define GAP 5)


(define (convert i)
  (cond [(= i 1) "blue"]
        [(= i 2) "red"]
        [(= i 3) "green"]
        [(= i 4) "orange"]
        [(= i 5) "pink"]
        [(= i 6) "brown"]
        [(= i 7) "violet"]
        [(= i 8) "purple"]
        [(= i 9) "cyan"]
        [(= i 10) "lightgreen"]
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

;; shift-axis-x : Number -> Number
(define (shift-axis-x x)
    (+ x FONT-SIZE-AXIS))

;; shift-axis-y : Number -> Number
(define (shift-axis-y y)
    (+ y FONT-SIZE-AXIS))

;; plot-x-axis : List<Number> -> Image
(define (plot-x-axis xs x-step x-min)
  (foldl place-image (rectangle (+ SIZE (* 2 FONT-SIZE-AXIS)) 
                                (+ FONT-SIZE-AXIS 5) 0 
                                "transparent")
         (map label xs) (map shift-axis-x (map (convert-x x-step x-min) xs))
         (make-list (length xs)(+ 5 (/ FONT-SIZE-AXIS 2)))))  

;; plot-y-axis : List<Number> -> Image
(define (plot-y-axis ys y-step y-min)
  (foldl place-image (rectangle AXIS-WIDTH 
                                (+ SIZE (* 2 FONT-SIZE-AXIS)) 0 
                                "transparent")
         (map label ys) (make-list (length ys) (/ AXIS-WIDTH 2))
         (map shift-axis-y (map (convert-y y-step y-min) ys))))  

;; -------------------------------------------------------------------
;; func : Function Number Number String -> Function
(define (func f start end label)
  (lambda (l-color)
    (let* [(step (/ (- end start) STEPS))
           (x (range start (+ end step) step))
           (y (map f x))
           (linecolor (convert l-color))]           
        (cons FUNC (cons label (cons linecolor (map make-posn x y)))))))

;; function : Function Number Number String -> Function
(define (function f start end linecolor label)
  (lambda (_count) ;not used here for anything
    (let* [(step (/ (- end start) STEPS))
           (x (range start (+ end step) step))
           (y (map f x))]           
        (cons FUNC (cons label (cons linecolor (map make-posn x y)))))))

;; convert-data : List<posn/list/vector> -> List<posn>
(define (convert-data list-data)
  (cond [(and (list? list-data)
              (vector? (first list-data)))
         (let [(l-data (map vector->list list-data))]
           (map make-posn (map first l-data)(map second l-data)))]
        [(posn? list-data) 
         (list list-data)]
        [(and (list? list-data)(posn? (first list-data)))
         list-data]
        [(and (list? list-data) (not (list? (first list-data))))
         (list (make-posn (first list-data)(second list-data)))]
        [(list? (first list-data))
             (map make-posn (map first list-data)(map second list-data))]
        [else list-data]))

;; convert-values : List -> List<posn>
(define (convert-values list-data)
  (let [(xs (range 0 (length list-data) 1))]
    (map (lambda (x y)(make-posn x y)) xs list-data)))
         
         
;; dots : Number Number String -> Function
(define (dots list-data start-x end-x start-y end-y label)
  (lambda (l-color)
    (let [(linecolor (convert l-color))
          (posns (convert-data list-data))]           
        (cons DOTS-OPEN (cons label (cons linecolor (cons (make-posn start-x start-y)
                                                    (cons (make-posn end-x end-y)
                                                          posns))))))))

;; points-help : Number Number String -> Function
(define (points-help list-data start-x end-x start-y end-y point-color point-type label show? line?)
  (lambda (_count) ;not used here for anything
   (let [(posns (convert-data list-data))]           
     (cond [(image? point-type)   ; image stored in point-type
           (cons IMAGE (cons label (cons point-type (cons (make-posn start-x start-y)
                                                                   (cons (make-posn end-x end-y)
                                                                    posns)))))] 
           [(and (string? point-type)(string=? point-type "solid") show? line?)
            (cons LINE-DOTS-SOLID-SHOW (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                   (cons (make-posn end-x end-y)
                                                                    posns)))))]
          [(and (string? point-type)(string=? point-type "solid") show?)
           (cons DOTS-SOLID-SHOW (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                   (cons (make-posn end-x end-y)
                                                                    posns)))))]
          [(and (string? point-type)(string=? point-type "solid") line?)
           (cons LINE-DOTS-SOLID (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                   (cons (make-posn end-x end-y)
                                                                    posns)))))]
          [(and (string? point-type)(string=? point-type "solid"))
           (cons DOTS-SOLID (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                (cons (make-posn end-x end-y)
                                                                      posns)))))]
          [(and (string? point-type)(string=? point-type "outline") show? line?)
           (cons LINE-DOTS-OPEN-SHOW (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                   (cons (make-posn end-x end-y)
                                                                    posns)))))]          
          [(and (string? point-type)(string=? point-type "outline") line?)
           (cons LINE-DOTS-OPEN (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                   (cons (make-posn end-x end-y)
                                                                    posns)))))]
           [(and (string? point-type)(string=? point-type "outline") show?)
           (cons DOTS-OPEN-SHOW (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                   (cons (make-posn end-x end-y)
                                                                    posns)))))]          

          [(and (string? point-type)(string=? point-type "outline"))
           (cons DOTS-OPEN (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                    (cons (make-posn end-x end-y)
                                                                          posns)))))]
          [else
           (cons LINE (cons label (cons point-color (cons (make-posn start-x start-y)
                                                                    (cons (make-posn end-x end-y)
                                                                          posns)))))]))))

;; points : Number Number String Boolean -> Function
(define (points list-data start-x end-x start-y end-y point-color point-type label)
  (points-help list-data start-x end-x start-y end-y point-color point-type label #f #f))

;; labeled-points : Number Number String Boolean -> Function
(define (labeled-points list-data start-x end-x start-y end-y point-color point-type label)
  (points-help list-data start-x end-x start-y end-y point-color point-type label #t #f))

;; convert-y : Number Number -> Number
(define (convert-y y-step y-min)
  (lambda (y)
    (- SIZE (* y-step (- y y-min)))))

;; convert-x : Number Number -> Number
(define (convert-x x-step x-min)
  (lambda (x)
    (* x-step (- x x-min))))

;; plot-help : List<Posn> Image Color -> Image
(define (plot-help posn-list target linecolor x-step y-step x-min x-max y-min y-max)
  (if (or (empty? posn-list)
          (< (length posn-list) 2))
      target
      (plot-help (rest posn-list)
            (add-line target
                      ((convert-x x-step x-min) (posn-x (first posn-list)))
                      ((convert-y y-step y-min) (posn-y (first posn-list)))
                      ((convert-x x-step x-min) (posn-x (second posn-list)))
                      ((convert-y y-step y-min) (posn-y (second posn-list)))
                      linecolor)
            linecolor x-step y-step x-min x-max y-min y-max)))

;; location->label : Number Number -> String
(define (location->label x y)
  (text (string-append "("
                       (number->string x)
                       ", "
                       (number->string y)
                       ")")
        (* 0.6 FONT-SIZE)
        "black"))
  
;; plot-dots : List<Posn> Image Image -> Image
(define (plot-dots posn-list target dot x-step y-step x-min x-max y-min y-max show?)
  (if (or (empty? posn-list)
          (< (length posn-list) 1))
      target
      (plot-dots (rest posn-list)
            (let* [(dot-x (posn-x (first posn-list)))
                   (dot-y (posn-y (first posn-list)))
                   (image-with-dot (place-image dot 
                                                ((convert-x x-step x-min) dot-x)
                                                ((convert-y y-step y-min) dot-y)
                                                target))]
              (if show?
                  (place-image (location->label dot-x dot-y)
                               (+ ((convert-x x-step x-min) dot-x)
                                  15)
                               (+ ((convert-y y-step y-min) dot-y)
                                  10)
                               image-with-dot)
                  image-with-dot))
              dot x-step y-step x-min x-max y-min y-max show?)))

;; plot-color : Number Number Number Number Number -> Function
(define (plot-with-color x-step y-step x-min x-max y-min y-max)
  (lambda (posn-list target)
    (cond [(= (first posn-list) IMAGE)
           (plot-dots (rest (rest (rest (rest (rest posn-list)))))
                      target 
                      (third posn-list) 
                      x-step 
                      y-step 
                      x-min 
                      x-max 
                      y-min 
                      y-max
                      #f)]
          [(= (first posn-list) FUNC)
           (plot-help (rest (rest (rest posn-list))) 
                      target 
                      (third posn-list) 
                      x-step 
                      y-step 
                      x-min 
                      x-max 
                      y-min 
                      y-max)]
          [(or (= (first posn-list) DOTS-SOLID)
               (= (first posn-list) DOTS-SOLID-SHOW)
               (= (first posn-list) DOTS-OPEN)
               (= (first posn-list) DOTS-OPEN-SHOW))
           (plot-dots (rest (rest (rest (rest (rest posn-list)))))
                      target 
                      (circle DOT-SIZE (if (or (= (first posn-list) DOTS-SOLID)
                                               (= (first posn-list) DOTS-SOLID-SHOW))
                                           "solid"
                                           "outline")
                              (third posn-list)) 
                      x-step 
                      y-step 
                      x-min 
                      x-max 
                      y-min 
                      y-max
                      (or (= (first posn-list) DOTS-SOLID-SHOW)
                          (= (first posn-list) DOTS-OPEN-SHOW)))]
          [(or (= (first posn-list) LINE-DOTS-SOLID)
               (= (first posn-list) LINE-DOTS-SOLID-SHOW)
               (= (first posn-list) LINE-DOTS-OPEN)
               (= (first posn-list) LINE-DOTS-OPEN-SHOW))
           (let [(plotted-dots (plot-dots (rest (rest (rest (rest (rest posn-list)))))
                                          target 
                                          (circle DOT-SIZE (if (or (= (first posn-list) LINE-DOTS-SOLID)
                                                                   (= (first posn-list) LINE-DOTS-SOLID-SHOW))
                                                               "solid"
                                                               "outline")
                                                  (third posn-list)) 
                                          x-step 
                                          y-step 
                                          x-min 
                                          x-max 
                                          y-min 
                                          y-max
                                          (or (= (first posn-list) LINE-DOTS-SOLID-SHOW)
                                              (= (first posn-list) LINE-DOTS-OPEN-SHOW))))]
              (plot-help (rest (rest (rest (rest (rest posn-list))))) ;remove the min and max points 
                      plotted-dots 
                      (third posn-list) 
                      x-step 
                      y-step 
                      x-min 
                      x-max 
                      y-min 
                      y-max))]
          ;; LINE:
          [else (plot-help (rest (rest (rest (rest (rest posn-list))))) ;remove the min and max
                           target 
                           (third posn-list) 
                           x-step 
                           y-step 
                           x-min 
                           x-max 
                           y-min 
                           y-max)])))
             
;; plot2D-help : List<Posn> String String String -> Image
(define (plot2D-help list-of-func x-min x-max y-min y-max)
  (let [(x-step (/ SIZE (- x-max x-min)))
        (y-step (/ SIZE (- y-max y-min)))]
    (if (list? list-of-func)
        (foldl (plot-with-color x-step y-step x-min x-max y-min y-max) BG list-of-func)
        ((plot-with-color x-step y-step x-min x-max y-min y-max) list-of-func BG))))

;; plot-label : List -> Image
(define (plot-label func img)
  (let [(icon (cond [(= (first func) IMAGE)
                     empty-image]
                    [(or (= (first func)FUNC)
                         (= (first func) LINE-DOTS-SOLID)
                         (= (first func) LINE-DOTS-SOLID-SHOW)
                         (= (first func) LINE-DOTS-OPEN)
                         (= (first func) LINE-DOTS-OPEN-SHOW)
                         (= (first func) LINE))
                     (rectangle 35 1.5 "solid" (third func))]
                    [(= (first func) SERIE)
                     (square (* 2 DOT-SIZE) "solid" (third func))]
                    [else
                     (circle DOT-SIZE (if (= (first func) DOTS-SOLID) 
                                       "solid" 
                                       "outline") 
                          (third func))]))
        (strlen (string-length (second func)))]
  (if (zero? strlen)
      img
      (if (< strlen 10)
          (above (overlay/xy icon
                             -80 -8
                             (overlay/xy (text (second func) 13 "black")
                                         -4 -3
                                         (label-box 10)))
                 img)
          (above (overlay/xy icon
                             (- (* strlen 8)) -8
                             (overlay/xy (text (second func) 13 "black")
                                         -4 -3
                                         (label-box strlen)))
                 img)))))
                
;; plot-labels : List -> Image
(define (plot-labels list-of-func)
  (if (list? list-of-func)
      (foldl plot-label empty-image list-of-func)
      (plot-label empty-image list-of-func)))

(define (plot2D-axis list-of-func x-min x-max y-min y-max arrows?)
  (let* [(x-step (/ SIZE (- x-max x-min)))
         (y-step (/ SIZE (- y-max y-min)))
         (plotted (add-axes (frame (plot2D-help list-of-func x-min x-max y-min y-max)) x-step y-step x-min y-min))        
         (x-values (axis x-min x-max))
         (y-values (axis y-min y-max))
         (y-axis-marks (create-y-axis y-values y-step y-min))
         (x-axis-marks (create-x-axis x-values x-step x-min))
         (marked-plot (above x-axis-marks
                             (beside y-axis-marks plotted y-axis-marks)
                             x-axis-marks))]
    (if arrows?
     (overlay/xy (plot-y-axis y-values y-step y-min)
                 AXIS-WIDTH (- (- AXIS-HEIGHT FONT-SIZE-AXIS))
                 (above (add-arrows marked-plot x-step y-step x-min y-min)
                        (plot-x-axis x-values x-step x-min)))   
     (overlay/xy (plot-y-axis y-values y-step y-min)
                 AXIS-WIDTH (- (- AXIS-HEIGHT FONT-SIZE-AXIS))
                 (above marked-plot
                        (plot-x-axis x-values x-step x-min))))))

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

;; add-axes
;; adds lines only 
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

;; adds x-axis only 
(define (add-x-axis plots y-step y-min plot-widht)
  (scene+line plots
            0 
            ((convert-y y-step y-min) 0)
            plot-widht
            ((convert-y y-step y-min) 0)
            "black")) 

;; add-arrows
;; adds arrows 
(define (add-arrows plots x-step y-step x-min y-min)
  (let* [(xax1 0)
         (xay ((convert-y y-step y-min) 0))
         (xax2 SIZE)
         (yax ((convert-x x-step x-min) 0))
         (yay1 0)
         (yay2 SIZE)
         (arrow-head-up (triangle 14 "solid" "black"))
         (arrow-head-left (rotate -90 arrow-head-up))]
   (place-image arrow-head-left (+ xax2 15) (+ xay AXIS-HEIGHT)
                (place-image arrow-head-up (+ yax AXIS-HEIGHT) (+ yay1 6) plots))))

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

;(check-expect (find-max-x (list (cons "label" (cons 3 (list (make-posn 2 3)
;                                                            (make-posn 4 5)
;                                                            (make-posn 10 0))))))
;              10)
;
;(check-expect (find-max-x (list (cons "label" (cons 3 (list (make-posn 2 3)
;                                                            (make-posn 4 5)
;                                                            (make-posn 10 0))))
;                                (cons "label" (cons 3 (list (make-posn 2 3)
;                                                            (make-posn 40 5)
;                                                            (make-posn 10 0))))))
;              40)

;; plot2D : List-of-func/dots/function/points String String String -> Image
(define (plot2D list-of-f x-label y-label title)
  (let* [(list-of-func (add-line-colors list-of-f '() 1))
         (min-x (find-min-x list-of-func))
         (min-y (find-min-y list-of-func))
         (max-x (find-max-x list-of-func))
         (max-y (find-max-y list-of-func))
         (plotted-labels1 (plot-labels list-of-func))
         (plotted-labels (overlay plotted-labels1
                                 (rectangle (image-width plotted-labels1)
                                            (image-height plotted-labels1)
                                            180
                                            "white")))
         (plots (overlay/xy (frame plotted-labels) -80 -20
                            (plot2D-axis list-of-func min-x max-x min-y max-y #f)))
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
    
;; plot2D-with-axes : List-of-func/dots/function/points String String String -> Image
(define (plot2D-with-axes list-of-f x-label y-label title)
  (let* [(list-of-func (add-line-colors list-of-f '() 1))
         (min-x (find-min-x list-of-func))
         (min-y (find-min-y list-of-func))
         (max-x (find-max-x list-of-func))
         (max-y (find-max-y list-of-func))
         ;; added for new arrowlabels
         (x-step (/ SIZE (- max-x min-x)))
         (y-step (/ SIZE (- max-y min-y)))
         (xax1 0)
         (xay ((convert-y y-step min-y) 0))
         (xax2 SIZE)
         (yax ((convert-x x-step min-x) 0))
         (yay1 0)
         (yay2 SIZE)
         ;;
         (plotted-labels1 (plot-labels list-of-func))
         (plotted-labels (overlay plotted-labels1
                                 (rectangle (image-width plotted-labels1)
                                            (image-height plotted-labels1)
                                            180
                                            "white")))
         (plots (overlay/xy (frame plotted-labels) -80 -20
                            (plot2D-axis
                             list-of-func min-x max-x min-y max-y #t)))
         (y-text (text y-label (* 0.8 FONT-SIZE) "black"))
         (x-text (text x-label (* 0.8 FONT-SIZE) "black"))
         (plot-marginals (* 10 AXIS-HEIGHT)) 
         (plots-f (overlay plots
                            (rectangle (+ (image-width plots) plot-marginals)
                                       (+ (image-height plots) plot-marginals)
                                       "solid"
                                       "transparent")))
         (x-offset (/ (- (image-width plots) SIZE (* 2 AXIS-HEIGHT)) 2))
         (title (text title FONT-SIZE "black"))
         (x-middle (/ (image-width plots-f) 2))]
         (place-image title 
                      (+ x-middle x-offset) 
                      AXIS-HEIGHT
                      (place-image y-text
                                   (+ yax AXIS-WIDTH (/ plot-marginals 2) (* 1.5 AXIS-HEIGHT))
                                   (+ yay1 (/ plot-marginals 2) (- (/ FONT-SIZE 2)))
                                   (place-image x-text
                                                (+ xax2
                                                   AXIS-WIDTH 
                                                   plot-marginals)
                                                (+ xay (/ plot-marginals 2) (* 1.3 AXIS-HEIGHT))
                                                plots-f)))))          

;; plot : List-of-func/dots/function/points String String String -> Image
(define (plot list-of-f x-label y-label title)
  (plot2D list-of-f x-label y-label title))

;; plot-with-axes : List-of-func/dots/function/points String String String -> Image
(define (plot-with-axes list-of-f x-label y-label title)
  (plot2D-with-axes list-of-f x-label y-label title))


;; ------------bar-chart -------------------------------------------------------------

;; draw-uni-bar : Number Number Color -> Image
(define (draw-uni-bar y-min y-max y-step bar-w gap)
  (lambda (value color image)
      (if (>= value 0)
        (beside/align "top" (above (rectangle bar-w (* y-step (- y-max value)) "solid" "transparent")   
                                   (rectangle bar-w (* y-step value) "solid" color)
                                   (rectangle bar-w (* y-step (abs y-min)) "solid" "transparent"))
                      (square gap "solid" "transparent")
                      image)
        (beside/align "top" (above (rectangle bar-w (* y-step y-max) "solid" "transparent")   
                                   (rectangle bar-w (* y-step (abs value)) "solid" color)
                                   (rectangle bar-w (* y-step (- (abs y-min)(abs value))) "solid" "transparent"))
                      (square gap "solid" "transparent")
                      image))))
    
;; draw-uni-bars : List Number Number Number Number -> Image
(define (draw-uni-bars y-min y-max y-step bar-w)
  (lambda (func image)
    (foldr (draw-uni-bar y-min y-max y-step bar-w SERIE-GAP)
           empty-image
           (fourth func)
           (make-list  (length (fourth func)) (third func)))))

;; draw-uni-bars-one-set : 
(define (draw-uni-bars-one-set y-min y-max y-step bar-w)
  (lambda (list-values color-values image)
    (foldl (draw-uni-bar y-min y-max y-step bar-w GAP)
           image
           list-values
           color-values)))

;; draw-uni-bars-series :
(define (draw-uni-bars-series y-min y-max y-step bar-w list-of-func img i series)  
  (if (>= i series)
      img
  (draw-uni-bars-series y-min y-max y-step bar-w
                        list-of-func 
                        (beside/align "top"
                                      img
                                      ((draw-uni-bars-one-set y-min y-max y-step bar-w)
                                              (map (lambda (x)(list-ref x i))(map fourth list-of-func))
                                             (map third list-of-func)
                                             empty-image)
                                      (square 15 "solid" "transparent"))
                        (add1 i)
                        series)))

(define (count-bars list-of-series)
  (if (list? list-of-series)
      (apply + (map length (map fourth list-of-series)))
      (length (fourth list-of-series))))

  ;; bar-chart-help : List<Numbers/List> Number Number Boolean -> Image
(define (bar-chart-help list-of-func y-min y-max bar-w series vertical?)
  (let* [(y-step (/ SIZE (- y-max y-min)))]
         (if (list? (first list-of-func))
             (draw-uni-bars-series y-min y-max y-step bar-w list-of-func empty-image 0 series)
             (foldl (draw-uni-bars y-min y-max y-step bar-w) empty-image list-of-func)))) 

(define (find-min list-of-series)
  (if (list? list-of-series)
      (apply min (flatten (map fourth list-of-series)))
      (apply min (fourth list-of-series))))
             
(define (find-max list-of-series)
  (if (list? list-of-series)
      (apply max (flatten (map fourth list-of-series)))
      (apply max (fourth list-of-series))))

(define (add-mark-x-pixel x-step image-width)
  (lambda(x img)
    (add-line img
              x
              0
              x
              10
              "black")))

(define (create-x-axis-pixel xs x-step image-width)
  (foldl (add-mark-x-pixel x-step image-width) (rectangle image-width AXIS-HEIGHT 0 "transparent") xs))

;; plot-x-axis-labels : List<Number> -> Image
(define (plot-x-axis-labels xs label-data image-w x-step)
  (let* [(labels (map (lambda (x)(text (cond [(number? x)(number->string x)]
                                     [(string? x) x]
                                     [else ""])
                               FONT-SIZE "black")) label-data))
        (max-width (apply max (map image-width labels)))]
    (if (< max-width x-step)
        (foldl place-image (rectangle image-w 
                                      (+ FONT-SIZE-AXIS 10) 0 
                                      "transparent")
               labels
               xs
               (make-list (length xs)(+ 5 (/ FONT-SIZE-AXIS 2))))
        (foldl place-image (rectangle image-w 
                                      (+ max-width 10) 0 
                                      "transparent")
               (map (lambda (x) (rotate 90 x)) labels)
               xs
               (map (lambda (x)(+ (/ x 2) 10)) (map image-width labels))))))

(define (fix-x-values number-of-bars number-of-sets x-step plot-width)
  (let [(count (/ number-of-bars number-of-sets))]
   ; (display count)(display "count")
   ; (display number-of-bars)(display "bars")
   ; (display x-step)(display "step")(display plot-width)(display "plot")
    (if (<= count 1)
        (range (+ (/ x-step 2) GAP) 
               plot-width     
               (+ x-step (* 3 GAP)))
        (range (+ (* count (/ x-step 2)) GAP) 
               plot-width     
               (+ (* count x-step) (* 3 GAP))))))

(define (data-ok? list-of-func data-labels)
     (andmap (lambda (x) (eq? x (length data-labels)))
             (map length (map fourth list-of-func))))

;; bar-chart : Serie/List-of-serie List<String/Number> String String Boolean -> Image
(define (bar-chart list-of-series label-data y-min y-max x-label y-label title vertical?)
  (let [(list-of-func (add-line-colors list-of-series '() 1))]
    (if (data-ok? list-of-func label-data)
            (let* [(number-of-bars (count-bars list-of-func))
                   (bar-w (/ SIZE number-of-bars))
                   (series (length label-data))
                   (y-values (axis y-min y-max))
                   (x-step (+ bar-w GAP))
                   (y-step (/ SIZE (- y-max y-min)))
                   (plotted-bars (frame  (beside (square 10 "solid" "transparent")
                                                 (bar-chart-help list-of-func
                                                                 (min (find-min list-of-func) y-min)
                                                                 (max (find-max list-of-func) y-max)
                                                                 bar-w
                                                                 series
                                                                 vertical?))))
                   (plot-width (image-width plotted-bars))
                   (plotted-bars-zero (add-x-axis plotted-bars y-step y-min plot-width)) 
                   (y-axis-marks (create-y-axis y-values y-step y-min))
                   (fixed-x-values (fix-x-values (count-bars list-of-func) (length label-data) x-step plot-width))
                   (x-axis-marks (create-x-axis-pixel fixed-x-values x-step plot-width))
                  ; (display x-axis-marks)
                   (marked-plot (above (beside y-axis-marks plotted-bars-zero y-axis-marks)
                                       x-axis-marks))
                   (plotted-labels1 (plot-labels list-of-func))
                   (plotted-labels (overlay plotted-labels1
                                            (rectangle (image-width plotted-labels1)
                                                       (image-height plotted-labels1)
                                                       180
                                                       "white")))
                   (plots (overlay/xy (frame plotted-labels) -80 -20
                                      (overlay/xy (plot-y-axis y-values y-step y-min)
                                                  AXIS-WIDTH (- (- AXIS-HEIGHT (* 1.7 FONT-SIZE-AXIS)))
                                                  (above marked-plot
                                                         (plot-x-axis-labels fixed-x-values label-data plot-width x-step)))))
                   (plots-f (overlay plots
                                     (rectangle (+ (image-width plots)(* 8 AXIS-HEIGHT))
                                                (+ (image-height plots)(* 8 AXIS-HEIGHT))
                                                "solid"
                                                "transparent")))
                   (y-offset (/ FONT-SIZE-AXIS 2))
                   (x-offset (/ (- (image-width plots) plot-width (* 2 AXIS-HEIGHT)) 2))
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
                                                     plots-f))))
            "Check your data. Each serie must have equal number of entries and there must be a label for each entry.")))          
  
(define (serie list-of-data color label)
  (lambda(_count); not used
    (cons SERIE (cons label (cons color (cons list-of-data '()))))))

(define (lines list-data start-x end-x start-y end-y color point-type label)
          (points-help list-data start-x end-x start-y end-y color point-type label #f #t))
    
(define (lines-labeled-points list-data start-x end-x start-y end-y color point-type label)
      (points-help list-data start-x end-x start-y end-y color point-type label #t #t))
 
(define (images list-data start-x end-x start-y end-y image)
  (points-help list-data start-x end-x start-y end-y "transparent" image "" #f #f))

