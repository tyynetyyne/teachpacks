#lang racket
(require plot)

(provide plot2D
         func)

; color converter for default colors
(define (color-map i)
  (cond [(= i 1) 3]
        [(= i 2) 1]
        [(= i 3) 2]
        [else i]))

; removes unnecessary labels and convest default colors
(define (function-helper f x0 xn i label)
  (if (string=? "" label)
      (function f x0 xn #:color (color-map i))
      (function f x0 xn #:color (color-map i) #:label label)))
        
; plot2D : Procedure/List-of-procedures String String String -> Image
(define (plot2D f [x-label "x"][y-label "y"][title ""])
  (let [(functions (convert-function-list '() f 1))]
    (if (and (not (empty? functions))
             (cons? functions)
             (> (length functions) 1))
        (plot (list (axes) functions)
                    #:title title	 
                    #:x-label x-label	 
                    #:y-label y-label)
        (plot functions 
              #:title title	 
              #:x-label x-label	 
              #:y-label y-label))))
  
; func : Function Number Number String -> List-of-(Function Number Number String)
(define (func fkt x0 xn [label ""])
  (list fkt x0 xn label))

; convert-function-list : List-of(List-of-(Function Number Number String)) -> <what ever is ok for plot>
(define (convert-function-list converted unconverted i)
  (cond[(empty? unconverted)
        converted]
       [(cons? (first unconverted))
        (convert-function-list (cons (function-helper (first (first unconverted))
                                                      (second (first unconverted))
                                                      (third (first unconverted))
                                                      i
                                                      (fourth (first unconverted)))
                                     converted)
                               (rest unconverted)
                               (add1 i))]
       [else
        (function-helper (first unconverted)
                         (second unconverted)
                         (third unconverted)
                         i
                         (fourth unconverted))]))