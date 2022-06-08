#lang r5rs

(#%require (only racket/base error))

(#%provide railswitch-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: Railswitch object
; __Procedures__:
; __Comment__:
; =======================================

(define (railswitch-adt)
  (let ((RAILSWITCH-ID 'default-id)
        (RAILSWITCH-POS 0)
        (RAILSWITCH-STATUS 0);holds the next node number location of the switch
        (SWITCH-STATUS-NUMBER 1)
        (RAILSWITCH-POS-VEC (make-vector 2 'default))
        (PREVIOUS-NODE 0)
        (x 0)
        (y 0))

    (define (new id rail-pos x1 y1)
      (set! RAILSWITCH-ID id)
      (set! RAILSWITCH-POS rail-pos)
      (set! x x1)
      (set! y y1)
      dispatch)

    (define (set-railswitch-pos-vec! pos1 pos2 prev)
      (vector-set! RAILSWITCH-POS-VEC 0 pos1);pos1 and pos2 are numbers representing their location in the graph
      (vector-set! RAILSWITCH-POS-VEC 1 pos2)
      (set! RAILSWITCH-STATUS pos1)
      (set! PREVIOUS-NODE prev))
  
    (define (switch! number);number values represent a node number (position in the graph)
      (cond ((eq? number (vector-ref RAILSWITCH-POS-VEC 0)) (set! RAILSWITCH-STATUS number)
                                                            (set! SWITCH-STATUS-NUMBER 1)
                                                            RAILSWITCH-STATUS)
            ((eq? number (vector-ref RAILSWITCH-POS-VEC 1)) (set! RAILSWITCH-STATUS number)
                                                            (set! SWITCH-STATUS-NUMBER 2)
                                                            RAILSWITCH-STATUS)
            (else
             (error "Cannot change switch status. Expected a switch-node-number. Given " number))))

    (define (not-used-node)
      (if (eq? RAILSWITCH-STATUS (vector-ref RAILSWITCH-POS-VEC 0))
          (vector-ref RAILSWITCH-POS-VEC 1)
          (vector-ref RAILSWITCH-POS-VEC 0)))
    
    (define (dispatch msg)
      (cond ((eq? msg 'new) new)
            ((eq? msg 'id) RAILSWITCH-ID)
            ((eq? msg 'railswitch-pos) RAILSWITCH-POS)
            ((eq? msg 'set-railswitch-pos-vec!) set-railswitch-pos-vec!)
            ((eq? msg 'previous-node) PREVIOUS-NODE)
            ((eq? msg 'SWITCH-STATUS-NUMBER) SWITCH-STATUS-NUMBER)
            ((eq? msg 'current-next-node) RAILSWITCH-STATUS) ; gives the next node symbol currently in the switch
            ((eq? msg 'not-used-node) (not-used-node))
            ((eq? msg 'switch!) switch!)
            ((eq? msg 'type) 'railswitch)
            ((eq? msg 'x) x)
            ((eq? msg 'y) y)
            (else (error ": not supported: ") msg)))
    dispatch))