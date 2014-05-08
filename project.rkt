
#lang racket/gui
(require racket/draw)

(define (make-2d-vector r c init)
  (build-vector r (lambda (x) (make-vector c init))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))
(define image (read-bitmap "929.jpg"))
(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

(define current-posn (make-2d-vector 6 7 #f))

(define total-moves 0)
; Make a 800 x 650 frame
(define frame (new frame%
                   [label "Connect Four"]
                   [width 800]
                   [height 650]))
(define myframe (new frame% 
                     [label "CONNECT FOUR"]
                     [width 400]
                     [height 200]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EXTRA FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rawpanel (new vertical-panel%
                      [parent myframe]
                      [alignment '(center center)]
                      [horiz-margin 50]
                      [spacing 20]))

(define row1 (new horizontal-panel% 
                  [parent rawpanel]
                  [alignment '(center center)]))

(define row2 (new horizontal-panel% 
                  [parent rawpanel]
                  [alignment '(center center)]))



(define num1 (new button% 
                  [parent row1]
                  [label "DOUBLE PLAYER"]
                  [min-width 150] 
                  [min-height 50]
                  [callback (lambda (button event)
                              (send myframe show #f)
                              (set! mode 2)
                              (send frame show #t))]))
(define num2 (new button% 
                  [parent row1]
                  [label "SINGLE PLAYER"]
                  [min-width 150] 
                  [min-height 50]
                  [callback (lambda (button event)
                              (send myframe show #f)
                              (set! mode 1)
                              (send frame show #t))]))

(define num3 (new button%
                  [parent row2]
                  [label "EXIT"]
                  [min-width 150]
                  [min-height 50]
                  [callback (lambda (button event)
                              (send myframe show #f))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mode #f);if mode=1, then its one player game , if mode=2 then 2 player



; Make the drawing area with a paint callback

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (if (nav-win current-posn)
          (if (= (nav-win current-posn) 1) (send msg set-label "PLAYER 2 WINS !!!") (send msg set-label "PLAYER 1 WINS !!!"))
          (if (eq? mode 1) (begin (fun (send event button-down? 'left) (send event get-x)) (if (nav-win current-posn)
                                                                                               (if (= (nav-win current-posn) 1) (send msg set-label "PLAYER 2 WINS !!!") (send msg set-label "PLAYER 1 WINS !!!"))
                                                                                               (comp-fun)))
              (fun (send event button-down? 'left) (send event get-x))
              )))    
    ; Call the superclass init, passing on all init args
    (super-new)))




(define canvas
  (new my-canvas%
       [parent frame]
       [paint-callback
        (lambda (canvas dc) (paint dc))]))

; ... pens, brushes, and draw-face are the same as above ...

(define (paint dc) 
                   
                   (send dc draw-bitmap face-bitmap 0 0))

; ... pens, brushes, and draw-face are the same as above ...

; Create a 800 x 650 bitmap
(define face-bitmap (make-object bitmap% 800 650))      ; Create a drawing context for the bitmap

(define bm-dc (make-object bitmap-dc% face-bitmap))     ; A bitmap's initial content is undefined; clear it before drawing
;(send bm-dc clear)



; Make some pens and brushes
(define blue-pen (make-object pen% "GREY" 4 'solid))
(define blue-pen1 (make-object pen% "GREY" 10 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))

(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-brush (make-object brush% "RED" 'solid))

; Define a procedure to draw a face
(define player 0);if player=0 its red's turn else yellow's
(send bm-dc draw-bitmap image 0 0)
(define (draw-face dc r c)

  (send canvas refresh)
    
  (send dc set-pen blue-pen)
  (if(= player 0) (begin (send dc set-brush red-brush) (send msg set-label "player1's turn"))
     (begin (send dc set-brush yellow-brush) (send msg set-label "player2's turn")))
  (if r (begin  (set! total-moves (add1 total-moves)) (play-sound "start.wav" #t) (send dc draw-ellipse (+ 120 (* 80 c)) (- 480 (* 80 r)) 80 80))
      (set! player player))
  
  

  (send dc set-pen blue-pen1)  
  (send dc set-brush no-brush)
  (send dc draw-rounded-rectangle 120 80 560 480 10)
  (define (h-lines n1)
    (if(= n1 5) (send dc draw-line (+ 200 (* 80 n1)) 80 (+ 200 (* 80 n1)) 560)
       
       (begin (send dc draw-line (+ 200 (* 80 n1)) 80 (+ 200 (* 80 n1)) 560) (send dc draw-line 120 (+ 160 (* 80 n1)) 680 (+ 160 (* 80 n1))) (h-lines (add1 n1)))))
  (h-lines 0))




; Show the frame
(send myframe show #t)

(define (fun click? mouse-x)
  (if click? (let* ([c (cond [(and (>= mouse-x 120) (< mouse-x 200)) 0]
                             [(and (>= mouse-x 200) (< mouse-x 280)) 1]
                             [(and (>= mouse-x 280) (< mouse-x 360)) 2]
                             [(and (>= mouse-x 360) (< mouse-x 440)) 3]
                             [(and (>= mouse-x 440) (< mouse-x 520)) 4]
                             [(and (>= mouse-x 520) (< mouse-x 600)) 5]
                             [(and (>= mouse-x 600) (< mouse-x 680)) 6])]
                    [r (fun3 c 0)])
               (if (void? r) (set! r #f) (draw-face bm-dc r c)))
      (draw-face bm-dc #f #f)))

(define (fun3 c r)
  (cond [(> r 5) (set! player player)]
        [(not (2d-vector-ref current-posn r c)) (begin (2d-vector-set! current-posn r c player) (draw-face bm-dc r c) (set! player (if(= player 0) 1 0)))]
        [else (fun3 c (+ 1 r))]))


(define msg (new message% [parent frame]
                 [label "No events so far..."]))



(define (comp-fun)
  (if (eq? player 1) (let* ((obm (best-move-by-minimax-level-2 current-posn))
                            (r (fun31 obm 0))) (if (void? r) (set! r #f) (draw-face bm-dc r obm))) (set! player 0)))

(define (fun31 c r)
  (cond [(> r 5) (set! player player)]
        [(not (2d-vector-ref current-posn r c)) (begin (2d-vector-set! current-posn r c player) (draw-face bm-dc r c) (set! player (if(= player 0) 1 0)))]
        [else (fun31 c (+ 1 r))]))

(define (make-virtual-move 2dv1 c r player1)                ;returns the 2dvector after the given move
  (define posn-vec (build-vector 6 (lambda (x) (build-vector 7 (lambda (y) (2d-vector-ref 2dv1 x y))))))
  (cond [(> r 5) #f]
        [(not (2d-vector-ref posn-vec r c)) (begin (2d-vector-set! posn-vec r c player1) posn-vec)]
        [else (make-virtual-move posn-vec c (+ 1 r) player1)]))


(define (legal-moves-generator 2dv)                          ;gives list of possible values of c
  (define v1 (vector-ref 2dv 5))
  (define (helper n)
    (if (= n 7) '()
        (append (if (vector-ref v1 n) '() (list n)) (helper (add1 n)))))
  (helper 0))

(define (roots 2dv list-of-moves pl)
  (define len (length list-of-moves))
  (define (helper n lom)
    (if (= n len) '()
        (cons (make-virtual-move 2dv (car lom) 0 pl) (helper (add1 n) (cdr lom)))))
  (helper 0 list-of-moves))



(define (min-subscore 2dv submove-pl)
  (define lom (legal-moves-generator 2dv))
  (define list-of-scores (map (lambda (x) (eval (make-virtual-move 2dv x 0 submove-pl) 1)) lom))
  (apply min list-of-scores))

(define (eval 2dv pl)     ;we are evaluating from the point of view of pl
  (define sum 0)
  (begin (if (nav-win 2dv) (if (eq? (nav-win 2dv) pl) (set! sum 500) (set! sum -500)) (set! sum sum))
         (set! sum (+ sum (exf 2dv pl)))
         sum))

(define (exf 2dv pl)
  (define score 0)
  (define (helper1 m1)
    (if (= m1 6) score (begin (helper2 (vector-ref 2dv m1) 0 m1) (helper1 (add1 m1)))))
  (define (helper2 vec m2 n2)
    (cond [(= m2 7) (set! score score)]
          [else (if (vector-ref vec m2) (begin (if (= (vector-ref vec m2) pl) (set! score (+ score (- 6 (+ (abs (- 3 n2)) (abs (- 3 m2)))))) (set! score (- score (- 6 (+ (abs (- 3 n2)) (abs (- 3 m2))))))) (helper2 vec (add1 m2) n2)) (helper2 vec (add1 m2) n2))]))  
  (helper1 0))

(define (list-find x l)
  (if (eq? x (car l)) 0 (add1 (list-find x (cdr l)))))

(define (best-score-by-minimax 2dv)
  (define lom (legal-moves-generator 2dv))
  (define scores (map (lambda (x) (min-subscore (make-virtual-move 2dv x 0 1) 0)) lom))
  
  (define man-score (apply max scores))
  man-score
  )

(define (best-move-by-minimax-level-2 2dv)
  (define lom (legal-moves-generator 2dv))
  
  (define list-of-possible-moves-by-player2 (map (lambda (x) (define 2dv1 (make-virtual-move 2dv x 0 1))
                                                   (define lom1 (legal-moves-generator 2dv1))
                                                   (map (lambda (y) (make-virtual-move 2dv1 y 0 0)) lom1)) lom)); basically its a list of lists of 2dvs   
  (define l1 (map (lambda (x1) (apply min (map (lambda (aa) (best-score-by-minimax aa)) x1))) list-of-possible-moves-by-player2))
  (define max-score (apply max l1))
  
  (list-ref lom (list-find max-score l1)))


(define (nav1 vic i)
  (cond [(and (number? (vector-ref vic i))
              (number? (vector-ref vic (+ i 1)))
              (number? (vector-ref vic (+ i 2)))
              (number? (vector-ref vic (+ i 3))))
         (cond [(= (vector-ref vic i)
                   (vector-ref vic (+ i 1))
                   (vector-ref vic (+ i 2))
                   (vector-ref vic (+ i 3)) 1) 1]
               [(= (vector-ref vic i)
                   (vector-ref vic (+ i 1))
                   (vector-ref vic (+ i 2))
                   (vector-ref vic (+ i 3)) 0) 0]
               
               [else #f])]
        [else #f]))
(define (nav2 vic i j)
  (cond [(and (number? (vector-ref (vector-ref vic i) j))
              (number? (vector-ref (vector-ref vic (+ i 1)) j))
              (number? (vector-ref (vector-ref vic (+ i 2)) j))
              (number? (vector-ref (vector-ref vic (+ i 3)) j)))
         (cond[(= (vector-ref (vector-ref vic i) j)
                  (vector-ref (vector-ref vic (+ i 1)) j)
                  (vector-ref (vector-ref vic (+ i 2)) j)
                  (vector-ref (vector-ref vic (+ i 3)) j) 1) 1]
              [(= (vector-ref (vector-ref vic i) j)
                  (vector-ref (vector-ref vic (+ i 1)) j)
                  (vector-ref (vector-ref vic (+ i 2)) j)
                  (vector-ref (vector-ref vic (+ i 3)) j) 0) 0]
              [else #f])]
        [else #f]))
(define (nav3 vic i j)
  (cond [ (and (number? (vector-ref (vector-ref vic i) j))
               (number? (vector-ref (vector-ref vic (+ i 1)) (+ j 1)))
               (number? (vector-ref (vector-ref vic (+ i 2)) (+ j 2)))
               (number? (vector-ref (vector-ref vic (+ i 3)) (+ j 3))))
          (cond [ (= (vector-ref (vector-ref vic i) j)
                     (vector-ref (vector-ref vic (+ i 1)) (+ j 1))
                     (vector-ref (vector-ref vic (+ i 2)) (+ j 2))
                     (vector-ref (vector-ref vic (+ i 3)) (+ j 3)) 1) 1]
                [ (= (vector-ref (vector-ref vic i) j)
                     (vector-ref (vector-ref vic (+ i 1)) (+ j 1))
                     (vector-ref (vector-ref vic (+ i 2)) (+ j 2))
                     (vector-ref (vector-ref vic (+ i 3)) (+ j 3)) 0) 0]
                [else #f])]
        [else #f]))
(define (nav4 vic i j)
  (cond [ (and (number? (vector-ref (vector-ref vic i) j))
               (number? (vector-ref (vector-ref vic (- i 1)) (+ j 1)))
               (number? (vector-ref (vector-ref vic (- i 2)) (+ j 2)))
               (number? (vector-ref (vector-ref vic (- i 3)) (+ j 3))))
          (cond [ (= (vector-ref (vector-ref vic i) j)
                     (vector-ref (vector-ref vic (- i 1)) (+ j 1))
                     (vector-ref (vector-ref vic (- i 2)) (+ j 2))
                     (vector-ref (vector-ref vic (- i 3)) (+ j 3)) 1) 1]
                [ (= (vector-ref (vector-ref vic i) j)
                     (vector-ref (vector-ref vic (- i 1)) (+ j 1))
                     (vector-ref (vector-ref vic (- i 2)) (+ j 2))
                     (vector-ref (vector-ref vic (- i 3)) (+ j 3)) 0) 0]
                [else #f])]
        [else #f]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (nav-win victor)
  (define (win-h vic i j)
    (cond [(and (<= i 5) (< j 3)) (if (number? (nav1 (vector-ref vic i) j)) (nav1 (vector-ref vic i) j) (win-h vic i (+ j 1)))]
          [(and (< i 5) (= j 3)) (if (number? (nav1 (vector-ref vic i) j)) (nav1 (vector-ref vic i) j) (win-h vic (+ i 1) 0))]
          [(and (= i 5) (= j 3)) (if (number? (nav1 (vector-ref vic i) j)) (nav1 (vector-ref vic i) j) #f)]))
  (define (winn-h vic i j)
    (cond [(and (< i 3) (<= j 6)) (if (number? (nav2 vic i j)) (nav2 vic i j) (winn-h vic (+ i 1) j))]
          [(and (= i 3) (<= j 5)) (if (number? (nav2 vic 0 (+ j 1))) (nav2 vic 0 (+ j 1)) (winn-h vic 1 (+ j 1)))]))
  (define (winne-h vic i j)
    (cond [(and (< i 2) (< j 4)) (if (number? (nav3 vic i j)) (nav3 vic i j) (winne-h vic (+ i 1) j))]
          [(and (= i 2) (< j 4)) (if (number? (nav3 vic i j)) (nav3 vic i j) (winne-h vic 0 (+ j 1)))]))
  (define (winner-h vic i j)
    (cond [(and (> i 3) (< j 4)) (if (number? (nav4 vic i j)) (nav4 vic i j) (winner-h vic (- i 1) j))]
          [(and (= i 3) (< j 4)) (if (number? (nav4 vic i j)) (nav4 vic i j) (winner-h vic 5 (+ j 1)))]))
  (cond [(= total-moves 42) (begin (send msg set-label "DRAW") (sleep/yield 900))]
        [(number? (win-h victor 0 0)) (win-h victor 0 0)]
        [(number? (winn-h victor 0 0)) (winn-h victor 0 0)]
        [(number? (winne-h victor 0 0)) (winne-h victor 0 0)]
        [(number? (winner-h victor 5 0)) (winner-h victor 5 0)] 
        [else #f]))


