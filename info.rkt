#lang info

(define collection "teachpacks")

(define version 0.5)

(define deps '("base" "htdp-lib"))

(define build-deps '("scribble-lib"
                     "scribble-doc"))

(define scribblings '(("doc/racket_turtle.scrbl" (multi-page))))
