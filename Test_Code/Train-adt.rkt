#lang r5rs

(#%require (only racket/base error))
(#%require "ID-handler-adt.rkt")

(#%provide train-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: train object
; __Procedures__:
; __Comment__:
; =======================================

(define (train-adt prev-node current-node)
  (let* ((TRAIN-ID 'default-id)
         (PREVIOUS-NODE prev-node)
         (CURRENT-NODE current-node)
         (CURRENT-SEGMENT 'default)
         (SPEED 0)
         (route '())
         (destination 0)
         )
    (define (new id-handler)
      (set! TRAIN-ID (id-handler 'add-object)))

    ;this needs some more work so it can work together with id-handler
    (define (new-id-given id)
      (set! TRAIN-ID id))
    
    (define (remove-train! id-handler)
      ((id-handler 'remove-train!) TRAIN-ID))
    
    (define (set-current-node! node)
      (set! CURRENT-NODE node))
    
    (define (set-previous-node! node)
      (set! PREVIOUS-NODE node))

    (define (set-speed! number)
      (set! SPEED number))

    (define (direction-sign)
      (if (>= SPEED 0)
          +
          -))
    
    (define (set-route! r)
      (set! route r))
    
    (define (update-route!);popping the already traversed nodes from te list
      (cond ((and (not (destination?))
                 (not (null? route))
                 (not (eq? CURRENT-NODE (car route)))) (set-route! (cdr route))
                                                            (update-route!))))
    
    (define (give-next-switch-pos)
      (if (not (null? route))
          (car route)))
    
    (define (destination?)
      (eq? CURRENT-NODE destination))
    
    (define (set-destination! dest)
      (set! destination dest))
    
    (define (set-current-segment! id)
      (set! CURRENT-SEGMENT id))
      
    (define (dispatch msg)
      (cond ((eq? msg 'new) new)
            ((eq? msg 'new-id-given) new-id-given)
            ((eq? msg 'id) TRAIN-ID)
            ((eq? msg 'remove-train!) remove-train!)
            ((eq? msg 'set-current-node!) set-current-node!)
            ((eq? msg 'set-previous-node!) set-previous-node!)
            ((eq? msg 'previous-node) PREVIOUS-NODE)
            ((eq? msg 'current-node) CURRENT-NODE)
            ((eq? msg 'speed) SPEED)
            ((eq? msg 'set-speed!) set-speed!)
            ((eq? msg 'set-route!) set-route!)
            ((eq? msg 'set-destination!) set-destination!)
            ((eq? msg 'route) route)
            ((eq? msg 'direction-sign) (direction-sign))
            ((eq? msg 'destination?) destination?)
            ((eq? msg 'update-route!) (update-route!))
            ((eq? msg 'give-next-switch-pos) (give-next-switch-pos))
            ((eq? msg 'destination) destination)
            ((eq? msg 'destination) set-destination!)
            ((eq? msg 'set-current-segment!) set-current-segment!)
            ((eq? msg 'current-segment) CURRENT-SEGMENT)
            (else (error ": not supported: " msg))))
    dispatch))
