#lang racket
(require 2htdp/image)

(provide display-with-units
         round-to-integer
         round-to-decimal)

;; round-to-integer : Number -> Number/Boolean
(define (round-to-integer x)
  (if (number? x)
      (if (positive? x)
          (inexact->exact (floor (+ x 0.5)))
          (inexact->exact (ceiling (- x 0.5))))
      #f))

;; round-to-decimals : Number Integer -> Number/Boolean
(define (round-to-decimal x decimals)
  (if (and (number? x)(integer? decimals))
      (exact->inexact (/ (round-to-integer (* x (expt 10 decimals))) (expt 10 decimals)))
      #f))

;; unit->image : String Number Number Color -> Image
(define (unit->image unit exp size color)
  (overlay/xy (unit-text->image unit size color)
              (image-width (unit-text->image unit size color)) 0
              (text exp (floor (/ size 2)) color)))

;; parse-exp : String -> String
(define (parse-exp str)
  (if (< (string-length str) 1)
      ""
      (substring str (sub1 (string-length str)))))

;; unit->image : String Size Color -> Image
(define (unit-text->image str size color)
  (text str size color))

;; exp? : String-> Boolean
(define (exp? str)
  (string->number str))

;; parse-unit : String -> String
(define (parse-unit str)
  (if (< (string-length str) 1)
      ""
      (substring str 0 (sub1 (string-length str)))))

;; display-with-units : String Size Color -> Image
(define (display-with-units str size color)
  (if (and (string? str)(> (string-length str) 0))
      (let [(l (string-split str))]
        (cond [(<= (length l) 1)
               empty-image]
              [(and (= (length l) 2)(exp? (parse-exp str)))
               (beside (text (first l) size color)
                       (unit->image (parse-unit (second l))
                                    (parse-exp (second l))
                                    size color))]
              [(= (length l) 2)
               (beside (text (first l) size color)
                       (unit-text->image (second l) size color))]
              [else empty-image]))
      empty-image))

