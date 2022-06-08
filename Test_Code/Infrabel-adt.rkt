#lang r5rs

(#%require (only racket/base error filter for-each car cons thread when sleep))
(#%require (prefix interface: "Simulator/interface.rkt"))
(#%require "Railway-adt.rkt")
(#%require "Train-adt.rkt")
(#%require "ID-handler-adt.rkt")
(#%require "Nmbs-adt.rkt")

(#%provide infrabel-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: Infrabel object
; __Procedures__:
; __Comment__:
; =======================================

(define (infrabel-adt file-str) ;"Simulator/setup_loop_and_switches.txt"
  (let ((railway (railway-adt))
        (ID-handler-Train (ID-handler-adt "T"))
        (train-lst '())
        (current-thread #f)
        (running? #f)
        (loop-wait 0.1))

    (define (start-simulation)
      ((railway 'load-setup) file-str)
      (interface:start-simulator)
      (set! running? #t)
      (set! current-thread (thread main-loop))
      )
    
    (define (stop-simulation)
      (set! running? #f)
      (interface:stop-simulator))
  
    (define (add-train! prev current . id) ;write id as a string, it will be converted to symbol and keep the uppercase/lowercase chars
      (let ((symbol-prev (string->symbol ((railway 'node-label) prev)))
            (symbol-current (string->symbol ((railway 'node-label) current)))
            (train (train-adt prev current)))
        (cond((null? id)((train 'new) ID-handler-Train)
                        (interface:add-loco (train 'id) symbol-prev symbol-current))
             (else ((train 'new-id-given) (string->symbol (car id)))
                   (interface:add-loco (train 'id) symbol-prev symbol-current)))
        (set! train-lst (cons train train-lst))))
    
    (define (remove-train! id)
      (interface:remove-loco id)
      (set! train-lst (filter (lambda (el)
                                (not (eq? id (el 'id))))
                              train-lst)))

    (define (train-speed id)
      (interface:get-loco-speed id))

    ;you can increase/decrease a trains speed by putting a positive or negative number (depending on direction)
    (define (increase/decrease-speed! train-adt number)
      (let ((new-speed (+ number
                          (interface:get-loco-speed (train-adt 'id)))))
        (interface:set-loco-speed! (train-adt 'id) new-speed)
        ((train-adt 'set-speed!) new-speed)))
    
    (define (set-train-speed!  train-adt number)
      (interface:set-loco-speed! (train-adt 'id) number)
      ((train-adt 'set-speed!) number))

    (define (railswitch-position railswitch-adt)
      (interface:get-switch-position (railswitch-adt 'id)))

    (define (change-switch! railswitch-adt number)
      ((railswitch-adt 'switch!) number)
      (interface:set-switch-position! (railswitch-adt 'id)
                                      (railswitch-adt 'SWITCH-STATUS-NUMBER)))

    (define (main-loop)
      (when running?
        (for-each update-train-and-switches! train-lst)
        (sleep loop-wait)
        (main-loop)))

    
    (define (update-train-and-switches! train)
      (let ((detect-id (interface:get-loco-detection-block (train 'id))))
        (cond ((and detect-id
                    (not (eq? detect-id
                         (train 'current-segment))))((railway 'update-train-pos!) train detect-id)
                                                    (train 'update-route!)
                                                    (let loop ((from (train 'previous-node))
                                                                (to  (train 'current-node))
                                                                (sign (train 'direction-sign))
                                                                (r (train 'route)))
                                                      (let ((node ((railway 'next/prev-node-label) from
                                                                                                  to
                                                                                                  sign))
                                                                 (edge ((railway 'next/prev-edge-label) from
                                                                                                  to
                                                                                                  sign)))
                                                          
                                                        (cond ((and (eq? 'railswitch  (node 'type))
                                                                    (not (null? (cddr r))))
                                                               (change-switch! node (caddr r))
                                                               (loop to (cadr r) sign (cdr r)))
                                                              ((eq? #\D (string-ref edge 0)) (cond ((eq? (train 'destination);we don't use destination? because of a bug in train.adt, it stops the train too soon 
                                                                                                         (train 'current-node));stop the train if destination reached
                                                                                                    (set-train-speed! train 0))))
                                                                                                 
                                                              (else
                                                               (loop to (cadr r) sign (cdr r))))))))))
  
    (define (dispatch msg)
      (cond ((eq? msg 'new) dispatch)
            ((eq? msg 'start-simulation) (start-simulation))
            ((eq? msg 'stop-simulation) (stop-simulation))
            ((eq? msg 'add-train!) add-train!)
            ((eq? msg 'remove-train!) remove-train!)
            ((eq? msg 'train-speed) train-speed)
            ((eq? msg 'increase/decrease-speed!) increase/decrease-speed!)
            ((eq? msg 'set-train-speed!) set-train-speed!)
            ((eq? msg 'railswitch-position) railswitch-position)
            ((eq? msg 'change-switch!) change-switch!)
            ((eq? msg 'train-lst) train-lst)
            ((eq? msg 'railway) railway)
            ((eq? msg 'get-loco-detection-block) interface:get-loco-detection-block)
            (else (error ": not supported: " msg))))
    dispatch))

(define infrabel (infrabel-adt "Simulator/setup_loop_and_switches.txt"))
(infrabel 'start-simulation)
(define nmbs (nmbs-adt))
((nmbs 'start) infrabel)