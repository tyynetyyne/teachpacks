#lang racket
(require plot)
(require lang/posn)

(provide plot2D
         func
         dots)

(define (convert i)
  (cond [(= i 1) 3]
        [(= i 2) 1]
        [(= i 3) 2]
        [else i]))

(define func-color 0)
(define dot-color 0)

(define (reset-colors)
  (begin (set! func-color 0)
         (set! dot-color 0)))

(define (get-color i type)
  (if (string=? type "func")
      (begin (set! func-color (add1 func-color))
             (convert func-color))
      (begin (set! dot-color (add1 dot-color))
             (convert dot-color))))

; convert-posn : List-of-posn -> List-of-vector
(define (convert-posn posn-list)
  (map vector (map posn-x posn-list)(map posn-y posn-list)))

; convert-list : List-of-list -> List-of-vector
(define (convert-list list-list)
  (map vector (map first list-list)(map second list-list)))

; dots : List-of-vectors/List-of-posn/List-of-list Number Number Number Number String -> Function
(define (dots coordinates [x0 #f] [xn #f] [y0 #f] [yn #f] [label ""])
  (local [(define xy (cond [(and (not (empty? coordinates))(posn? (first coordinates)))
                            (convert-posn coordinates)]
                           [(and (not (empty? coordinates))(cons? (first coordinates)))
                            (convert-list coordinates)]
                           [else coordinates]))]
          (lambda (x)(cond [(and x0 xn y0 yn (not (string=? "" label)))
                          (points xy #:x-min x0 #:x-max xn #:y-min y0 #:y-max yn #:label label #:color (get-color x "dots"))]
                         [(and x0 xn y0 yn)
                          (points xy #:x-min x0 #:x-max xn #:y-min y0 #:y-max yn #:color (get-color x "dots"))]
                         [else
                          (points xy #:color (get-color x "dots"))]))))

(define (func fkt x0 xn [label ""])
  (lambda (x) (if (string=? "" label)
                  (function fkt x0 xn #:color (get-color x "func"))
                  (function fkt x0 xn #:label label #:color (get-color x "func")))))

(define (add-colors to-be-plotted i final-list)
  (cond [(empty? to-be-plotted)
         final-list]
        [else
         (add-colors (rest to-be-plotted) (add1 i) (cons ((first to-be-plotted) i) final-list))]))

; plot2D : Procedure/List-of-procedures String String String -> Image
(define (plot2D to-be-plotted [x-label "x"][y-label "y"][title ""])
  (begin (reset-colors)
         (if (cons? to-be-plotted)
             (plot 
              (add-colors to-be-plotted 0 (list (axes)))
              #:title title	 
              #:x-label x-label	 
              #:y-label y-label)
             (plot 
               (list (axes)(to-be-plotted 0))
              #:title title	 
              #:x-label x-label	 
              #:y-label y-label))))

