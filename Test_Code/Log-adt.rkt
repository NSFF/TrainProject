#lang r5rs

(#%require (only racket/base error))

(#%provide log-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: Log object
; __Procedures__:
; __Comment__:
; =======================================

(define (log-adt)
  
  
  (define (dispatch msg)
    (cond ((eq? msg 'new) new)
          (else (error ": not supported: ") msg)))
    dispatch)