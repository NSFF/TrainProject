#lang r5rs

(#%require (only racket/base error))

(#%provide route-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: Route object
; __Procedures__:
; __Comment__:
; =======================================

(define (route-adt)
  
  
  (define (dispatch msg)
    (cond ((eq? msg 'new) new)
          (else (error ": not supported: ") msg)))
    dispatch)