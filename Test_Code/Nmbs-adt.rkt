#lang r5rs

(#%require (only racket/base error thread when sleep car))
(#%require "Railway-adt.rkt")
(#%require "Gui-adt.rkt")
(#%require (prefix bft: "lib/bft-applications.rkt"))

(#%provide nmbs-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: Nmbs object
; __Procedures__:
; __Comment__:
; =======================================

(define (nmbs-adt)
  (let ((current-thread #f)
        (running? #f)
        (loop-wait 0.1)
        (WINDOW 'default)
        (infrabel 'default))
    
    (define (start infrabel-adt)
      (set! infrabel infrabel-adt)
      (define gui (gui-adt "Nmbs-train-simulation" 1000 700))
      (set! WINDOW gui)
      ((gui 'initialize-window) (infrabel 'railway)
                                (infrabel 'train-lst)
                                (infrabel 'get-loco-detection-block))
      (set! running? #t)
      (set! current-thread (thread main-loop)))

    (define (stop)
      (set! running? #f))

    ;determines the shortest path from point A to B not considering the weight values.
    (define (make-route from destination)
      (let ((g ((infrabel 'railway) 'adj-lst)))
        (display (bft:shortest-path g from destination))
        (bft:shortest-path g from destination)))
    
    (define (main-loop)
      (when running?
        (cond ((WINDOW 'add-train?)
               ;adding a train
               ((infrabel 'add-train!) (WINDOW 'choice-previous)
                                       (WINDOW 'choice-current))
               ;setting up a route
               (((car (infrabel 'train-lst)) 'set-route!) (make-route (WINDOW 'choice-current) (WINDOW 'choice-destination)))
               ;initializing destination of the train
               (((car (infrabel 'train-lst)) 'set-destination!) (WINDOW 'choice-destination))
               ;increasing the start speed of the train
               ((infrabel 'increase/decrease-speed!) (car (infrabel 'train-lst))
                                                     (WINDOW 'choice-speed))
               ;adding the train to the combo-field
               ((WINDOW 'append-train!) (symbol->string ((car (infrabel 'train-lst)) 'id)))
               (WINDOW 'add-train-false!)))
        ((WINDOW 'update) (infrabel 'railway)
                          (infrabel 'train-lst)
                          (infrabel 'get-loco-detection-block))
        (sleep loop-wait)
        (main-loop)))
    
    (define (dispatch msg)
      (cond ((eq? msg 'new) dispatch)
            ((eq? msg 'start) start)
            ((eq? msg 'stop) (stop))
            (else (error ": not supported: " msg))))
    dispatch))