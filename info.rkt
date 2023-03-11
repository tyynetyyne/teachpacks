#lang info

(define collection "teachpacks")

(define version "0.5")

(define deps '("gui-lib"
               "base" "htdp-lib" "plot-gui-lib" "plot-lib"))

(define build-deps '("scribble-lib"
                     "scribble-doc"
                     "htdp-doc"
                     "racket-doc"))

(define scribblings '(("doc/racket_turtle.scrbl" (multi-page))))
