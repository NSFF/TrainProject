#lang racket

(#%require (only racket/base error))
(#%require racket/gui/base)
(#%require racket/draw)
(#%require racket/class)

(#%provide gui-adt)

; =======================================
; __Description__:
; __Parameters__:
; __Ouput__: Gui object
; __Procedures__:
; __Comment__:
; =======================================

(define (gui-adt title width height)
  (let ((FRAME 'default)
        (PANEL 'default)
        (CANVAS 'default)
        (BITMAP 'default)
        (BM-DC 'default)
        (RAILWAY-ADT 'default)
        (TRAIN-LST '())
        (SPHERE-SIZE 20)
        (ADD-TRAIN? #f)
        (CALIBRATION-FACTOR 50)
        (NODE-LIST '())
        (CURRENT-ACTIVE-TRAIN-LIST '())
        (COMBO-FIELD-TRAIN 'default)
        (SLIDER-SPEED 'default)
        (CHOICE-PREVIOUS 'default)
        (CHOICE-CURRENT 'default)
        (CHOICE-DESTINATION 'default))
    
    (define (initialize-window railway train-list get-loco-detection-block)
      (set! RAILWAY-ADT railway)
      (set! TRAIN-LST train-list)

      ; makes a list of string(integers) that represent the node numbers. (1 2 3 4 ...)
      (define (initialize-node-list)
        (define (iter n)
          (cond ((> n 0)
                 (set! NODE-LIST (cons (number->string n) NODE-LIST))
                 (iter (- n 1)))))
        (iter (- (RAILWAY-ADT 'order) 1)))
      
      (initialize-node-list)
      
      
      ; makes the window
      (set! FRAME (new frame%
                     [label title]
                     [width width]
                     [height height]))
      ; Horizontal panel
      (set! PANEL (new horizontal-panel% [parent FRAME]
                       [style '(border)]
                       [spacing 50]
                       [alignment '(center center)]))

      ; A bitmap where you can draw on
      (set! BITMAP (make-bitmap (- width 200) (- height 100)))
      ; Makes a drawing context of the bitmap
      (set! BM-DC (make-object bitmap-dc% BITMAP))
      
      ; makes a canvas ready to draw things on
      (set! CANVAS (new canvas% [parent FRAME]
                      [paint-callback ;paint-callback will draw everything mentioned below on startup
                       (lambda (canvas dc)
                         (send dc draw-bitmap BITMAP 0 0)
                         )]
                      [min-width (- width 300)]
                      [min-height (- height 200)]))
      
      ; Makes a button in the frame
      (new button% [parent PANEL]
           [label "Add"]
           ; Callback procedure for a button click:
           [callback (lambda (button event)
                       (set! ADD-TRAIN? #t))])

      (set! COMBO-FIELD-TRAIN (new combo-field%
                                  (label "current active trains")
                                  (choices CURRENT-ACTIVE-TRAIN-LIST)
                                  (parent PANEL)
                                  (init-value "no trains active")
                                  (style (list 'vertical-label))
                                  (horiz-margin 15)))
      
      ; Makes a dropdown menu
      (set! CHOICE-PREVIOUS (new choice%
                                   (label "Previous-node ")
                                   (parent PANEL)
                                   (choices NODE-LIST)
                                   (style (list 'vertical-label))))
      (set! CHOICE-CURRENT (new choice%
                                  (label "Current-node ")
                                  (parent PANEL)
                                  (choices NODE-LIST)
                                  (style (list 'vertical-label))))
      (set! CHOICE-DESTINATION (new choice%
                                    (label "Destination ")
                                    (parent PANEL)
                                    (choices NODE-LIST)
                                    (style (list 'vertical-label))))
      (set! SLIDER-SPEED (new slider%
                              (label "Speed")
                              (parent PANEL)
                              (min-value -100)
                              (max-value 100)
                              (init-value 0)
                              (horiz-margin 25)
                              (style (list 'horizontal 'vertical-label))))
      
      (initialize-railway BM-DC get-loco-detection-block); drawing the tracks and detection-blocks
      
      ; Show the window
      (send FRAME show #t)
      )
      

  
    (define (draw-edge-line dc node-1 node-2 pen-color)
      (define edge-readjusting
        (/ SPHERE-SIZE  2))
      (send dc set-pen pen-color)
      (send dc draw-line
            (+ (node-1 'x) edge-readjusting)
            (+ (node-1 'y) edge-readjusting)
            (+ (node-2 'x) edge-readjusting)
            (+ (node-2 'y) edge-readjusting)))
    
    (define (draw-switches dc)
      (define switch-pen (make-object pen% "BLUE" 5 'solid))
      (define no-switch-pen (make-object pen% "RED" 5 'solid))
      (define no-pen (new pen% [style 'transparent]))
      (define node-brush (new brush% [color "BLACK"]))
      (let loop ((n 1))
        (cond ((< n (RAILWAY-ADT 'order))
               (let ((node ((RAILWAY-ADT 'node-dispatch) n)))
                 (cond ((eq? 'railswitch (node 'type))
                        (let* ((prev-node-nr (node 'previous-node))
                               (prev-node ((RAILWAY-ADT 'node-dispatch) prev-node-nr))
                               (node-1-nr (node 'current-next-node))
                               (node-1 ((RAILWAY-ADT 'node-dispatch) node-1-nr))
                               (node-2-nr (node 'not-used-node))
                               (node-2 ((RAILWAY-ADT 'node-dispatch) node-2-nr)))
                          
                          ;drawing the updated edges
                          (draw-edge-line dc prev-node node switch-pen)
                          (draw-edge-line dc node node-1 switch-pen)
                          (draw-edge-line dc node node-2 no-switch-pen)

                          ;redraw the spheres above the switches
                          (send dc set-pen no-pen)
                          (send dc set-brush node-brush)
                          (send dc draw-ellipse (node 'x) (node 'y) SPHERE-SIZE SPHERE-SIZE)
                          (send dc draw-ellipse (node-1 'x) (node-1 'y) SPHERE-SIZE SPHERE-SIZE)
                          (send dc draw-ellipse (node-2 'x) (node-2 'y) SPHERE-SIZE SPHERE-SIZE)
                          (send dc draw-ellipse (prev-node 'x) (prev-node 'y) SPHERE-SIZE SPHERE-SIZE))))
                 (loop (+ n 1))))))
      (send CANVAS on-paint))
  
    (define (draw-train dc get-loco-detection-block)
      (define no-pen (new pen% [style 'transparent]))
      (define train-brush (new brush% [color "ORANGE"]))
      
      (define (draw train-adt)
        (cond ((get-loco-detection-block (train-adt 'id))
               (let ((node ((RAILWAY-ADT 'node-dispatch) (train-adt 'current-node))))
                 (send dc set-pen no-pen)
                 (send dc set-brush train-brush)
                 (send dc draw-ellipse (- (node 'x) 2.5) (- (node 'y) 2.5) (+ SPHERE-SIZE 5)  (+ SPHERE-SIZE 5))))))
      
      (for-each draw TRAIN-LST)
      (send CANVAS on-paint))
    
    (define (initialize-railway dc get-loco-detection-block)
      (send dc clear)


      (define track-pen (make-object pen% "BLACK" 5 'solid))
      (define detection-block-pen (make-object pen% "GREEN" 5 'solid))
      (define no-pen (new pen% [style 'transparent]))
      (define no-brush (new brush% [style 'transparent]))
      (define node-brush (new brush% [color "BLACK"]))
    
      (send dc set-smoothing 'aligned)

    
      (define (draw-nodes)
        (let loop ((n 1))
          (cond ((< n (RAILWAY-ADT 'order))
                 (let ((node ((RAILWAY-ADT 'node-dispatch) n)))
                   (send dc set-pen no-pen)
                   (send dc set-brush node-brush)
                   (send dc draw-ellipse (node 'x) (node 'y) SPHERE-SIZE SPHERE-SIZE)
                   (loop (+ n 1)))))))
    
      (define (draw-edges)
        (define (draw edge-symbol pen-color)
          (let* ((vec (RAILWAY-ADT edge-symbol))
                 (length (vector-length vec)))
            (let loop ((n 0))
              (cond ((< n length)(let* ((dispatch (vector-ref vec n))
                                        (node-1 ((RAILWAY-ADT 'node-dispatch) (dispatch 'from)))
                                        (node-2 ((RAILWAY-ADT 'node-dispatch) (dispatch 'to))))
                                   (draw-edge-line dc node-1 node-2 pen-color)
                                   (loop (+ n 1))))))))
        (draw 'detection-block-vec detection-block-pen)
        (draw 'track-vec track-pen))
    
      (draw-edges)
      (draw-nodes)
      
      (draw-switches dc)
      (draw-train dc get-loco-detection-block)
      
      (send CANVAS on-paint))

    (define (update railway train-list get-loco-detection-block)
      (set! TRAIN-LST train-list)
      (set! RAILWAY-ADT railway)
      (initialize-railway BM-DC get-loco-detection-block))

    (define (append-train! string)
      (send COMBO-FIELD-TRAIN append string))

    ;gets the selected string from the choice-menu and converts it a number
    (define (convert-selection-number choice)
      (string->number (list-ref NODE-LIST (send choice get-selection))))
    
  (define (dispatch msg)
    (cond ((eq? msg 'new) dispatch)
          ((eq? msg 'update) update)
          ((eq? msg 'add-train?) ADD-TRAIN?)
          ((eq? msg 'append-train!) append-train!)
          ((eq? msg 'choice-previous) (convert-selection-number CHOICE-PREVIOUS))
          ((eq? msg 'choice-current) (convert-selection-number CHOICE-CURRENT))
          ((eq? msg 'choice-destination) (convert-selection-number CHOICE-DESTINATION))
          ((eq? msg 'choice-speed) (/ (send SLIDER-SPEED get-value) CALIBRATION-FACTOR));we divide by 50 to calibrate the train speed visualy
          ((eq? msg 'add-train-false!) (set! ADD-TRAIN? #f))
          ((eq? msg 'initialize-window) initialize-window)
          (else (error ": not supported: " msg))))
    dispatch))

