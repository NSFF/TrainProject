#lang r5rs

(#%require (only racket/base error eof read-line car cdr cddr cadr caddr cadddr cdddr)) ;we import everything 1 by 1 because of an unknown error racket/base gives
(#%require racket/string)
(#%require (prefix graph-lst: "lib/adjacency-list.rkt"))
(#%require (prefix graph: "lib/adjacency-matrix.rkt"))
(#%require "Railswitch-adt.rkt")
(#%require "Train-adt.rkt")
(#%require "ID-handler-adt.rkt")
(#%require "Node-adt.rkt")
(#%require "track-adt.rkt")
(#%require "Detectionblock-adt.rkt")

(#%provide railway-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: Railway object
; __Procedures__:
; __Comment__:
; =======================================

(define (railway-adt)
  (let ((adj-graph '())
        (adj-lst 'adjacency-graph-lst)
        (detection-block-vec 'default) ;we will put detetctionblock-adt's into this vector
        (track-vec 'default)
        (str-offset 1)
        (ORDER-OF-GRAPH 0)); the offset in the string where the number is positioned
    
    (define (preprocessing-order-of-graph str)
      (let ((input-port (open-input-file str)))
        (define (read-loop order amount-of-detection-blocks amount-of-tracks)
          (let ((first-char (string-ref (read-line input-port) 0)))
            (cond ((eq? #\N first-char) (read-loop (+ order 1) amount-of-detection-blocks  amount-of-tracks))
                  ((eq? #\D first-char) (read-loop order (+ amount-of-detection-blocks 1)  amount-of-tracks))
                  ((eq? #\T first-char) (read-loop order amount-of-detection-blocks (+ amount-of-tracks 1)))
                  (else (close-input-port input-port)
                        (set! detection-block-vec (make-vector amount-of-detection-blocks 'default))
                        (set! track-vec (make-vector amount-of-tracks 'default))
                        (set! ORDER-OF-GRAPH order)
                        order))))
        (read-loop 1 0 0)))

  
    (define (load-setup str);"Simulator/setup_loop_and_switches.txt"
      (let* ((input-port (open-input-file str))
             (order (preprocessing-order-of-graph str)))
        (set! adj-graph (graph:new #f order))
        (set! adj-lst (graph-lst:new #f order))
        
        ; adding the nodes and edges of the the file line by line
        (define (load-into-graph line node)
          (let* ((str-lst (string-split line))
                 (category (car str-lst))
                 (label (cadr str-lst)))
            
            (define (abstract-add-edge! from to label)
              (graph:add-edge! adj-graph
                               (string->number (substring from str-offset)); from
                               (string->number (substring to str-offset)); to
                               label)
              ; a duplicate of the adj-graph-vector so we can use "shortest-path.rkt" in nmbs to determine the best route
              (graph-lst:add-edge! adj-lst
                                   (string->number (substring from str-offset))
                                   (string->number (substring to str-offset))))
        
            (cond ((string=? category "N") (if (eq? (string-ref label 0) #\A)
                                               (graph:label! adj-graph node (node-adt label
                                                                                      (string->number (caddr str-lst))
                                                                                      (string->number (cadddr str-lst))))
                                               (graph:label! adj-graph node (((railswitch-adt) 'new) (string->symbol label)
                                                                                                     node
                                                                                                     (string->number (caddr str-lst))
                                                                                                     (string->number (cadddr str-lst)))))); nodes
                  ((string=? category "D") (let ((node-1 (caddr str-lst))
                                                 (node-2 (cadddr str-lst)))
                                             (vector-set! detection-block-vec
                                                          (- (string->number (substring label str-offset)) 1)
                                                          (detectionblock-adt label
                                                                              (string->number (substring node-1 str-offset))
                                                                              (string->number (substring node-2 str-offset))))
                                             (abstract-add-edge! node-1 node-2 label))); detection blocks
                  ((string=? category "T") (abstract-add-edge! label (caddr str-lst) category)
                                           (let track-loop ((n 0))
                                             (if (and (> (vector-length track-vec) n)
                                                      (eq? 'default (vector-ref track-vec n)))
                                                 (vector-set! track-vec n (track-adt (string->number (substring label str-offset))
                                                                                     (string->number (substring (caddr str-lst) str-offset))))
                                                 (track-loop (+ n 1))))); tracks
                  
                  ; searching the right railswitch-adt and setting the 2 possible switch positions into it
                  ; afterwards we add the 2 edges 
                  ((string=? category "S") (((graph:label adj-graph (string->number (substring label str-offset))) 'set-railswitch-pos-vec!)
                                            (string->number (substring (cadddr str-lst) str-offset))
                                            (string->number (substring (caddr(cddr str-lst)) str-offset))
                                            (string->number (substring (caddr str-lst) str-offset)))
                                           
                                           (let switch-loop; switches
                                             ((node-2 (cddr str-lst)))
                                             (cond ((not (null? (cdr node-2)))
                                                    (abstract-add-edge! label (car node-2) label)
                                                    (switch-loop (cdr node-2)))))
                                           ;this is a temporary implementation, because we skip 1 edge in adj-graph and we still want it in our adj-lst
                                           (graph-lst:add-edge! adj-lst
                                                                (string->number (substring label str-offset))
                                                                (string->number (substring (cadr(cdddr str-lst)) str-offset))))
                  ((string=? category "L"))))) ;Preloaded trains should be implemented later on, we can add trains manualy now
        
        ;reads a line of the file each loop and calls the load-into-graph procedure
        (define (read-loop node)
          (let ((line (read-line input-port)))
            (cond ((eq? eof line))
                  (else (load-into-graph line node)
                        (read-loop (+ node 1))))))
        (read-loop 1)
        (close-input-port input-port)))

    (define (switch! graph-location)
      (let ((railswitch-adt (graph:label adj-graph graph-location)))
        (graph:delete-edge! adj-graph
                            (railswitch-adt 'railswitch-pos)
                            (railswitch-adt 'current-next-node))
        (graph:add-edge! adj-graph
                         (railswitch-adt 'railswitch-pos)
                         ((railswitch-adt 'switch!) (railswitch-adt 'not-used-node))
                         (railswitch-adt 'id))))

    (define (next/prev-node prev from next/prev)
      (graph:next/prev-node adj-graph prev from next/prev))
    
    (define (next/prev-node-label prev from next/prev)
      (graph:next/prev-node-label adj-graph prev from next/prev))

    (define (next/prev-edge-label prev from next/prev)
      (graph:next/prev-edge-label adj-graph prev from next/prev))
    
    (define (node-label number)
      ((graph:label adj-graph number) 'id))
    (define (node-dispatch number)
      (graph:label adj-graph number))

    (define (update-train-pos! train-adt detection-block-id)
      (let ((detectionblock-dispatch (vector-ref detection-block-vec
                                                 (- (string->number (substring (symbol->string detection-block-id)
                                                                            str-offset)) 1))))
        ((train-adt 'set-previous-node!) (detectionblock-dispatch 'from))
        ((train-adt 'set-current-node!) (detectionblock-dispatch 'to))
        ((train-adt 'set-current-segment!) detection-block-id)))
  
  
    (define (dispatch msg)
      (cond ((eq? msg 'new) dispatch)
            ((eq? msg 'load-setup) load-setup)
            ((eq? msg 'adj-graph) adj-graph)
            ((eq? msg 'adj-lst) adj-lst)
            ((eq? msg 'switch!) switch!)
            ((eq? msg 'next/prev-node) next/prev-node)
            ((eq? msg 'next/prev-node-label) next/prev-node-label)
            ((eq? msg 'next/prev-edge-label) next/prev-edge-label)
            ((eq? msg 'node-label) node-label)
            ((eq? msg 'node-dispatch) node-dispatch)
            ((eq? msg 'update-train-pos!) update-train-pos!)
            ((eq? msg 'order) ORDER-OF-GRAPH)
            ((eq? msg 'detection-block-vec) detection-block-vec)
            ((eq? msg 'track-vec) track-vec)
            (else (error ": not supported: " msg))))
    dispatch))
