#lang r5rs

(#%require (only racket/base error))
(#%require (only racket/string string-append*))
(#%require (prefix stack: "lib/linked-stack-adt.rkt")) ;this code was taken from Wolfang De Meuter

(#%provide id-handler-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: ID-handler object
; __Procedures__:
; __Comment__:
; =======================================

(define (id-handler-adt str)
  (let* ((LAST-NUMBER 0)
         (DELETED-OBJECTS-STACK (stack:new))
         (str-offset (+ (string-length str) 1)) ;used to know where the number is located in the symbol when popped from the stack
         )

    ; the LAST-NUMBER can be changed at will by adding a number
    (define (set-last-number! number)
      (set! LAST-NUMBER (+ LAST-NUMBER number)))

    ; returns the ID symbol of an object
    ; example: 'T-1
    (define (add-object)
      (define (make-id-symbol number)
        (string->symbol (string-append str "-"
                                       (number->string LAST-NUMBER))))
      
      (cond ((stack:empty? DELETED-OBJECTS-STACK)(set-last-number! 1);adding 1 to the last object number
                                                 (make-id-symbol (+ LAST-NUMBER 1)))
            (else (make-id-symbol (stack:pop! DELETED-OBJECTS-STACK)))))
    
    ; pops the ID-number on the stack ready to pop for the next new train who needs an ID
    (define (remove-object! ID-symbol)
      (stack:push! DELETED-OBJECTS-STACK (string->number (substring (symbol->string ID-symbol) str-offset))))
      
  
  (define (dispatch msg)
    (cond ((eq? msg 'add-object)(add-object))
          ((eq? msg 'remove-train!) remove-object!)
          (else (error ": not supported: " msg))))
    dispatch))