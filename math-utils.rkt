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
  (number? (string->number str)))

;; parse-unit : String -> String
(define (parse-unit str)
  (if (< (string-length str) 1)
      ""
      (substring str 0 (sub1 (string-length str)))))

;; string-split : String -> list-of-String
;; NOTE: needed in WeScheme (comment out in DrRacket)
;(define (string-split str)
;  (rec-string-split '() "" str))

;; rec-string-split : list-of-String String String -> list-of-string
;; splitting a string into a list of strings at each whitespace
;; NOTE: needed in WeScheme (comment out in DrRacket)
;(define (rec-string-split str-list curstr str)
;      (cond [(and (<= (string-length str) 0)
;                  (not (<= (string-length curstr) 0)))
;             (append str-list (list curstr))]
;            [(<= (string-length str) 0)
;             str-list]
;            [(and (string=? (substring str 0 1) " ")
;                  (> (string-length curstr) 0))
;             (rec-string-split (append str-list (list curstr)) "" (substring str 1))]
;            [(string=? (substring str 0 1) " ")
;             (rec-string-split str-list curstr (substring str 1))]
;            [else
;             (rec-string-split str-list (string-append curstr (substring str 0 1)) (substring str 1))]))

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

