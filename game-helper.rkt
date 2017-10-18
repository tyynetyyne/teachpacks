;; ---------------------------------------------------------------------------------------------------
;; Basic Game - Helper functions (ISL) 
;;
;; This file contains helperfuntions, which are needed to display some extra information including 
;; current coordinates of the sprites, box or triangle images for collision detection 
;; 
;; 
;; Tiina Partanen
;; Version 0.1
;; ---------------------------------------------------------------------------------------------------
#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(provide draw-helper-image)

;; check-y : Number Number Boolean -> Number
(define (check-y y height mathstyle?)
  (if mathstyle?
      (- height y)
      y))
  
; draw-image2 : Posn Image Image Boolean -> Image
(define (draw-image2 p img background mathstyle?)
    (let ((h (image-height background)))
      (place-image img
                   (posn-x p)
                   (check-y (posn-y p) h mathstyle?)
                   background)))

;; move-info : Posn -> Posn
(define (move-info p)
  (make-posn (- (posn-x p) 40)
             (posn-y p)))
  
;; draw? : Posn Number Number -> Boolean
(define (draw? p width height)
  (and (< 0 (posn-x p) width)
       (< 0 (posn-y p) height)))

;; draw-position : Posn Number String -> Image
(define (draw-position p size color)
  (text (string-append "(" (number->string (posn-x p)) ", " (number->string (posn-y p)) ")") 
        size 
        color))

;; draw-triangle : Posn Posn Image String Boolean -> Image
(define (draw-triangle p1 p2 background color mathstyle?)
  (let ((h (image-height background)))
    (add-line (add-line (add-line background (posn-x p1) 
                                  (check-y (posn-y p1) h mathstyle?)
                                  (posn-x p2) 
                                  (check-y (posn-y p2) h mathstyle?)
                                  color)
                        (posn-x p1) 
                        (check-y (posn-y p1) h mathstyle?)                               
                        (posn-x p1) 
                        (check-y (posn-y p2) h mathstyle?)
                        color)
              (posn-x p2) 
              (check-y (posn-y p2) h mathstyle?)
              (posn-x p1) 
              (check-y (posn-y p2) h mathstyle?) 
              color)))

;; draw-pythagoras : Posn Posn Posn Image String Boolean -> Image
(define (draw-pythagoras player danger target background color mathstyle?)
  (let ((w (image-width background))
        (h (image-height background)))
    (cond [(and (draw? player w h) (draw? target w h) (draw? danger w h))
           (draw-triangle player target
                          (draw-triangle player danger background color mathstyle?)
                          color mathstyle?)]
          [(and (draw? player w h) (draw? target w h))
           (draw-triangle player target background color mathstyle?)]
          [(and (draw? player w h) (draw? danger w h))
           (draw-triangle player danger background color mathstyle?)]
          [else background])))

;; draw-box : Number Number Posn Image String Boolean -> Image
(define (draw-box space-x space-y pos background color mathstyle?)
  (let ((w (image-width background))
        (h (image-height background)))
  (cond [(draw? pos w h)
         (draw-image2 pos (rectangle (* 2 space-x) (* 2 space-y) "outline" color) background mathstyle?)]
        [else
         background])))
                          
;; draw-coordinates : Posn Image Number String Boolean-> Image
(define (draw-coordinates pos background size color mathstyle?)
  (draw-image2 (move-info pos) (draw-position pos size color) background mathstyle?))

;; draw-positions : Posn Posn Posn Image Number String Boolean -> Image
(define (draw-positions player danger target background size color mathstyle?)
  (let ((w (image-width background))
        (h (image-height background)))
    (cond [(and (draw? player w h) (draw? danger w h) (draw? target w h))
           (draw-coordinates target
                             (draw-coordinates danger 
                                               (draw-coordinates player background size color mathstyle?) 
                                               size color mathstyle?)
                             size color mathstyle?)]
          [(and (draw? player w h) (draw? target w h))
           (draw-coordinates target 
                             (draw-coordinates player background size color mathstyle?) 
                             size color mathstyle?)]
          [(draw? player w h)
           (draw-coordinates player background size color mathstyle?)]
          [else 
           background])))

;; draw-helper-image : Posn Posn Posn Image Boolean Number String Number Number Boolean -> Image
(define (draw-helper-image player danger target background pythagoras? text-size color space-x space-y mathstyle?)
  (let ((positions-img (draw-positions player danger target background text-size color mathstyle?)))
    (cond [pythagoras?
           (draw-pythagoras player danger target positions-img color mathstyle?)]
          [else
           (draw-box space-x space-y player positions-img color mathstyle?)])))