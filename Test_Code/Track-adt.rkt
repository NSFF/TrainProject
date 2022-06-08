#lang r5rs

(#%require (only racket/base error))

(#%provide track-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: Track object
; __Procedures__:
; __Comment__:
; =======================================

(define (track-adt from to)
  
  
  (define (dispatch msg)
    (cond ((eq? msg 'from) from)
          ((eq? msg 'to) to)
          (else (error ": not supported: " msg))))
    dispatch)