;; Math-utils 0.4
;; - added mean, median, mode, frequency, frequencies
;; - added degrees->radians, radians->degrees
;; ---------------------------------------
#lang racket
(require 2htdp/image)

(provide quantity->image
         quantity-str->image
         ; term->image         ; todo
         round-to-integer
         round-to-decimal
         display-with-units  ; replaced by quantity-str->image
         units->image
         mean ;new
         median ;new
         mode ;new
         frequency ;new
         frequencies ;new
         degrees->radians ; new
         radians->degrees ; new
         )

;; degrees->radians : Number -> Number
(define (degrees->radians deg)
  (/ (* deg pi) 180))

;; radians->degrees : Number -> Number
(define (radians->degrees rad)
  (/ (* rad 180) pi))

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

;; unit->image : String String Number Color -> Image
(define (unit->image unit exp size color)
  (overlay/xy (unit-text->image unit size color)
              (image-width (unit-text->image unit size color)) 0
              (text exp (floor (/ size 2)) color)))

;; units->image : String Number Number Color -> Image/false
(define (units->image unit exp size color)
  (if (and (string? unit)(number? exp)(number? size)(< 0 size 256))
      (unit->image unit (number->string exp) size color)
      #f))

;; parse-exp : String -> String
(define (parse-exp str)
  (if (< (string-length str) 1)
      ""
      (substring str (sub1 (string-length str)))))

;; unit-text->image : String Size Color -> Image
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

;; display-with-units : String Integer Color -> Image/false
(define (display-with-units str size color)
  (if (and (string? str)(> (string-length str) 0)(number? size)(< 0 size 256))
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
      #f))

;; quantity-str->image : String Integer Color -> Image/false
(define (quantity-str->image str size color)
  (display-with-units str size color))

;; quantity->image : Number String Integer Integer Color -> Image/false
(define (quantity->image value units exp size color)
  (cond [(and (number? value)(string? units)(number? exp)(not (or (= exp 0)(= exp 1)))(number? size)(< 0 size 256))
         (beside (text (number->string value) size color)
                 (units->image units exp size color))]
        [(and (number? value)(string? units)(number? exp)(= exp 1)(number? size)(< 0 size 256))
         (beside (text (number->string value) size color)
                 (unit-text->image units size color))]
        [(and (number? value)(string? units)(number? exp)(= exp 0)(number? size)(< 0 size 256))
         (text (number->string value) size color)]
        [else #f]))

;; mean : List<Number> -> Number/False
(define (mean numbers)
  (if (not (empty? numbers))
      (/ (apply + numbers) (length numbers))
      #f))

;; median-helper : List<Number> -> Number
(define (median-helper numbers)
  (if (not (even? (length numbers))) 
          (list-ref numbers (floor (/ (length numbers) 2)))
          (mean (list (list-ref numbers (/ (length numbers) 2))
                      (list-ref numbers (sub1 (/ (length numbers) 2)))))))

;; median : List<Number> -> Number/False
(define (median numbers)
  (if (not (empty? numbers))
      (median-helper (sort numbers <))
      #f))

;; MODE
;; ---------------
;; same? : Number -> Boolean
(define (same? x)
  (lambda (y) (= x y)))

;; count : List<Number> -> Number
(define (count numbers)
  (length (filter (same? (first numbers)) numbers)))

;; cut-off : List<Number> -> List<Number>
(define (cut-off numbers)
  (remove* (list (first numbers)) numbers))

;; helper-mode : Number Number Number Number List<Number> -> Number/False
(define (helper-mode mode-now max-now numbers)
  (if (empty? numbers)
      mode-now
        (if (> (count numbers) max-now)
            (helper-mode (first numbers)(count numbers)(cut-off numbers))
            (helper-mode mode-now max-now (cut-off numbers)))))

;; mode : List<Number> -> Number/False
(define (mode numbers)
    (if (not (empty? numbers))
        (helper-mode #f 0 (sort numbers <))
        #f))

;; frequency : Number List<Number> -> Number
(define (frequency i numbers)
  (- (length numbers)
     (length (remove* (list i) numbers))))

;; frequencies : List<(List Number Number)> List<Number> -> List<(List Number Number)>
(define (frequencies-helper current numbers)
  (if (empty? numbers)
      current
      (if (empty? current)
          (frequencies-helper (list (list (first numbers) (frequency (first numbers) numbers)))
                              (cut-off numbers))
          (frequencies-helper (cons (list (first numbers)(frequency (first numbers) numbers))
                                    current)
                              (cut-off numbers)))))

;; frequencies : List<Numbers> -> List<(List Number Number)> / False
(define (frequencies numbers)
  (if (not (empty? numbers))
      (sort (frequencies-helper '() numbers)
            (lambda (x y) (> (second x)(second y))))
      #f))