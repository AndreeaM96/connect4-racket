#lang racket

(require racket/include)
(include "connect4-test.rkt")

(define RED 1)
(define YELLOW 2)
(define EMPTY 0)
(define INFINITY 99999)

;; Task pregătitor
(define init-state
  (λ (height width player)
    (list (init-board height width) player)
    ))

(define init-row
  (λ (width)
    (if (= 0 width)
        '()
        (cons EMPTY (init-row (- width 1))))
    ))

(define init-board
  (λ (height width)
    (if (= 0 height)
        '()
        (cons (init-row width) (init-board (- height 1) width))
        )
    ))

(define is-empty-board?
  (λ (board)
    (null?
     (filter (λ (x)
               (if (null? x)
                   #f
                   #t
                   ))
             (map (λ (row)
                    (filter (λ (elem)
                              (if (= elem EMPTY)
                                  #f
                                  #t
                                  ))
                            row))
                  board)))
    ))

(define get-height
  (λ (board)
    (foldl
     (λ (row count)
       (+ count 1)
       )
     0
     board)

    ))

(define get-width
  (λ (board)
    (foldl
     (λ (elem count)
       (+ count 1)
       )
     0
     (car board))

    ))

(define find-row
  (λ (board coord index)
    (if (= coord index)
        (car board)
        (find-row (cdr board) coord (- index 1))
        )
    ))

(define find-elem
  (λ (row coord index)
    (if (= coord index)
        (car row)
        (find-elem (cdr row) coord (+ index 1))
        )
    ))

(define get-disc
  (λ (board position)
    (find-elem (find-row board (cdr position) (- (get-height board) 1)) (car position) 0)
    ))

(define get-player
  (λ (state)
    (second state)
    ))

(define get-board
  (λ (state)
    (car state)
    ))

(define state-test
  (list (list '(0 0 1 0 0 0 0)
              '(0 0 2 1 0 0 0)
              '(0 0 1 2 0 0 0)
              '(0 0 1 1 0 1 0)
              '(0 0 2 1 0 2 2)
              '(0 0 1 2 1 2 2)) YELLOW)
  )

;; Task 1 a) - Determinarea acțiunilor posibile

(define transpose
  (λ (matrix)
    (apply map list matrix)))

(define find-col
  (λ (board coord index)
    (if (= coord index)
        (car board)
        (find-col (cdr board) coord (+ index 1))
        )
    ))

(define is-full?
  (λ (line)
    (null?
     (filter (λ (elem)
               (if (= elem EMPTY)
                   #t
                   #f
                   ))
             line))
    ))

(define get-available-actions
  (λ (board)
    (filter
     (λ (poz)
       (not (is-full? (find-col (transpose board) poz 0)))
       )
     (build-list (get-width board) identity))
    ))
  
;; Task 1 b) - Aplicarea unei acțiuni

(define add-element-at
  (λ (col elem)
    (if (= (car col) EMPTY)
        (cons elem (cdr col))
        (cons (car col) (add-element-at (cdr col) elem))
        )))
    
(define add-col-to-matrix
  (λ (matrix row pos index)
    (if (= pos index)
        (cons row (cdr matrix))
        (cons (car matrix) (add-col-to-matrix (cdr matrix) row pos (+ index 1)))
        )
    ))
    

(define apply-action
  (λ (state action)
    (list 
     (transpose (add-col-to-matrix (transpose (get-board state)) (reverse (add-element-at (reverse (find-col (transpose (get-board state)) action 0)) (get-player state))) action 0))

     (if (= (get-player state) YELLOW)
         RED
         YELLOW)
     )
    ))

(define apply-actions
  (λ (state actions)
    (if (null? actions)
        state
        (if (and (not (list? actions)) (not (pair? actions)))
            (apply-action state actions)
            (apply-actions (apply-action state (car actions)) (cdr actions))
            ))
    ))

;; Task 1 c) - Verificarea stării finale
(define is-board-full?
  (λ (board)
    (null?
     (filter (λ (row)
               (if (is-full? row)
                   #f
                   #t
                   ))
             board))
    ))

(define check-right
  (λ (board row player pos)
    (if (null? row)
        #f
        (if (< (- (- (get-width board) 1) pos) 3)
            #f
            (if (and (= (first row) player) (= (second row) player) (= (third row) player) (= (fourth row) player))
                #t
                (or (check-right board (cdr row) player (+ pos 1)) #f)
                )))))
                   
(define check-horizontal
  (λ (board player)
    (not (null?
          (filter
           (λ (row)
             (check-right board row player 0))
           board)
          ))
    ))

(define check-up
  (λ (board col player pos)
    (if (null? col)
        #f
        (if (< (- (- (get-width board) 1) pos) 3)
            #f
            (if (and (= (first col) player) (= (second col) player) (= (third col) player) (= (fourth col) player))
                #t
                (or (check-up board (cdr col) player (+ pos 1)) #f)
                )))))

(define check-vertical
  (λ (board player)
    (not (null?
          (filter
           (λ (col)
             (check-up (transpose board) (reverse col) player 0))
           (transpose board))
          ))
    ))

(define check-left-up
  (λ (board row player pos rowindex)
    (if (< rowindex 3)
        #f
        (if (null? row)
            #f
            (if (< pos 3)
                #f
                (if (and (= (car row) player) (= (list-ref (list-ref board (- rowindex 1)) (- pos 1)) player) (= (list-ref (list-ref board (- rowindex 2)) (- pos 2)) player) (= (list-ref (list-ref board (- rowindex 3)) (- pos 3)) player))
                    #t
                    (or (check-left-up board (cdr row) player (+ pos 1) rowindex) #f)
                    ))))))

(define check-right-up
  (λ (board row player pos rowindex)
    (if (< rowindex 3)
        #f
        (if (null? row)
            #f
            (if (< (- (- (get-width board) 1) pos) 3)
                #f
                (if (and (= (car row) player) (= (list-ref (list-ref board (- rowindex 1)) (+ pos 1)) player) (= (list-ref (list-ref board (- rowindex 2)) (+ pos 2)) player) (= (list-ref (list-ref board (- rowindex 3)) (+ pos 3)) player))
                    #t
                    (or (check-right-up board (cdr row) player (+ pos 1) rowindex) #f)
                    ))))))

(define check-left-down
  (λ (board row player pos rowindex)
    (if (< (- (- (get-height board) 1) rowindex) 3)
        #f
        (if (null? row)
            #f
            (if (< pos 3)
                #f
                (if (and (= (car row) player) (= (list-ref (list-ref board (+ rowindex 1)) (- pos 1)) player) (= (list-ref (list-ref board (+ rowindex 2)) (- pos 2)) player) (= (list-ref (list-ref board (+ rowindex 3)) (- pos 3)) player))
                    #t
                    (or (check-left-down board (cdr row) player (+ pos 1) rowindex) #f)
                    ))))))

(define check-right-down
  (λ (board row player pos rowindex)
    (if (< (- (- (get-height board) 1) rowindex) 3)
        #f
        (if (null? row)
            #f
            (if (< (- (- (get-width board) 1) pos) 3)
                #f
                (if (and (= (car row) player) (= (list-ref (list-ref board (+ rowindex 1)) (+ pos 1)) player) (= (list-ref (list-ref board (+ rowindex 2)) (+ pos 2)) player) (= (list-ref (list-ref board (+ rowindex 3)) (+ pos 3)) player))
                    #t
                    (or (check-right-down board (cdr row) player (+ pos 1) rowindex) #f)
                    ))))))

(define check-diagonal
  (λ (board player)
    (ormap
     (λ (row rowindex)
       (or (check-left-up board row player 0 rowindex) (check-right-up board row player 0 rowindex) (check-left-down board row player 0 rowindex) (check-right-down board row player 0 rowindex))
       )
     board (build-list (get-height board) identity))
    ))

(define check-win
  (λ (board)
    (if (check-horizontal board RED)
        RED
        (if (check-horizontal board YELLOW)
            YELLOW
            (if (check-vertical board RED)
                RED
                (if (check-vertical board YELLOW)
                    YELLOW
                    (if (check-diagonal board RED)
                        RED
                        (if (check-diagonal board YELLOW)
                            YELLOW
                            #f
                            ))))))
    ))

(define is-game-over?
  (λ (state)
    (if (is-board-full? (get-board state))
        3
        (if (equal? (check-win (get-board state)) RED)
            RED
            (if (equal? (check-win (get-board state)) YELLOW)
                YELLOW
                #f
                ))
        )))

;; Task 2 - Euristică simplă
(define select-random-action
  (λ (state rand-gen)
    (let ((rand (random (get-width (get-board state)) rand-gen)))
      (if (member rand (get-available-actions (get-board state)))
          rand
          (select-random-action state rand-gen)))
    ))

(define can-win-game?
  (λ (state)
    (let ((win-state (filter
                      (λ (action)
                        (if (is-game-over? (apply-action state action))
                            #t
                            #f
                            ))
                      (get-available-actions (get-board state)))))
      (if (null? win-state)
          #f
          (if (or (list? win-state) (pair? win-state))
              (car win-state)
              win-state))) 
    ))

(define can-enemy-win-game?
  (λ (state)
    (let* ((enemy-state (list (get-board state) (- 3 (get-player state)))) (enemy-win-state (filter
                                                                                             (λ (action)
                                                                                               (if (is-game-over? (apply-action enemy-state action))
                                                                                                   #t
                                                                                                   #f
                                                                                                   ))
                                                                                             (get-available-actions (get-board enemy-state)))))
      (if (null? enemy-win-state)
          #f
          (if (or (list? enemy-win-state) (pair? enemy-win-state))
              (car enemy-win-state)
              enemy-win-state))
      )
    ))

(define get-action
  (λ (state)
    (let ((my-action (can-win-game? state)) (enemy-action (can-enemy-win-game? state)))
      (if my-action
          my-action
          (if enemy-action
              enemy-action
              (select-random-action state (current-pseudo-random-generator)))))      
    ))

(define AI #t) ;Trebuie modificat în #t după ce se implementează play-game

(define get-action-list
  (λ (state strategy1 strategy2)
    (if (is-game-over? state)
        (list (is-game-over? state))
        (let ((action ((car strategy1) state (cdr strategy1))))
          (cons action (get-action-list (apply-action state action) strategy2 strategy1)))
        )))

(define play-game
  (λ (state strategy1 strategy2)
    (let ((result (get-action-list state strategy1 strategy2)))
      (cons (reverse (cdr (reverse result))) (car (reverse result))))
    ))

;; Bonus 
(define evaluate
  (λ (state)
    ;TODO
    'your-code-here))

(define negamax
  (λ (state maxDepth)
    ;TODO
    'your-code-here))
     
;; SECȚIUNE DE TESTARE - NU modificați această linie!
;; ATENȚIE! Pentru a primi punctaj pe temă, NU modificați această secțiune!
;;
;; CHECK - TASK 0 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 0 : 20 puncte) ;;check-exp
(check-exp-part 'is-empty-board?1 .02 (is-empty-board? (init-board 7 7)) #t)
(check-exp-part 'is-empty-board?2 .02 (is-empty-board? (get-board state-test)) #f)
(check-exp-part 'is-empty-board?3 .02 (is-empty-board? (get-board (init-state 7 8 RED))) #t)
(check-exp-part 'get-height1 .02 (get-height (get-board (init-state 7 8 YELLOW))) 7)
(check-exp-part 'get-height2 .02 (get-height (get-board state-test)) 6)
(check-exp-part 'get-height3 .02 (get-height (init-board 10 14)) 10)
(check-exp-part 'get-width1 .02 (get-width (get-board (init-state 7 8 YELLOW))) 8)
(check-exp-part 'get-width2 .02 (get-width (get-board state-test)) 7)
(check-exp-part 'get-width3 .02 (get-width (init-board 10 14)) 14)
(check-exp-part 'get-width4 .01 (get-width (init-board 20 20)) 20)
(check-exp-part 'get-player1 .02 (get-player state-test) YELLOW)
(check-exp-part 'get-player2 .02 (get-player (init-state 15 7 RED)) RED)
(check-exp-part 'get-player3 .02 (get-player (init-state 10 8 YELLOW)) YELLOW)
(check-exp-part 'get-disc1 .05 (get-disc (get-board (init-state 10 8 YELLOW)) (cons 1 2)) EMPTY)
(check-exp-part 'get-disc2 .05 (get-disc (get-board state-test) (cons 1 2)) EMPTY)
(check-exp-part 'get-disc3 .05 (get-disc (get-board state-test) (cons 5 0)) YELLOW)
(check-exp-part 'get-disc4 .05 (get-disc (get-board state-test) (cons 5 1)) YELLOW)
(check-exp-part 'get-disc5 .05 (get-disc (get-board state-test) (cons 5 2)) RED)
(check-exp-part 'get-disc6 .05 (get-disc (get-board state-test) (cons 5 3)) EMPTY)
(check-exp-part 'get-disc7 .05 (get-disc (get-board state-test) (cons 2 3)) RED)
(check-exp-part 'get-disc8 .05 (get-disc (get-board state-test) (cons 2 5)) RED)
(check-exp-part 'get-disc9 .05 (get-disc (get-board state-test) (cons 3 0)) YELLOW)
(check-exp-part 'get-disc10 .05 (get-disc (get-board state-test) (cons 3 3)) YELLOW)
(check-exp-part 'get-disc11 .05 (get-disc (get-board state-test) (cons 6 0)) YELLOW)
(check-exp-part 'get-disc12 .05 (get-disc (get-board state-test) (cons 6 1)) YELLOW)
(check-exp-part 'get-disc13 .05 (get-disc (get-board state-test) (cons 6 5)) EMPTY)
(check-exp-part 'get-disc14 .05 (get-disc (get-board state-test) (cons 0 0)) EMPTY)
(check-exp-part 'get-disc15 .05 (get-disc (get-board state-test) (cons 6 5)) EMPTY)
;; SFÂRȘIT CHECK - TASK 0 - NU modificați această linie!
;;
;; CHECK - Task1 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 1 : 50 puncte) ;;check-exp
(check-exp-part 'get-available-actions1 .04 (get-available-actions (get-board state-test)) '(0 1 3 4 5 6))
(check-exp-part 'get-available-actions2 .04 (get-available-actions (init-board 7 9)) '(0 1 2 3 4 5 6 7 8))
(check-exp-part 'get-available-actions3 .04 (get-available-actions (get-board (init-state 10 8 YELLOW))) '(0 1 2 3 4 5 6 7))
(check-exp-part 'get-available-actions4 .04 (get-available-actions (get-board (apply-action state-test 3))) '(0 1 4 5 6))
(check-exp-part 'get-available-actions5 .04 (get-available-actions (get-board (apply-actions state-test '(3 5 5 5)))) '(0 1 4 6))
(check-exp-part 'apply-action1 .02 (get-disc (get-board (apply-action state-test 3)) (cons 3 5)) YELLOW)
(check-exp-part 'apply-action2 .02 (get-player (apply-action state-test 3)) RED)
(check-exp-part 'apply-action3 .02 (get-player (apply-action (init-state 7 7 YELLOW) 1)) RED)
(check-exp-part 'apply-action4 .02 (get-disc (get-board (apply-action (init-state 6 6 RED) 1)) (cons 1 0)) RED)
(check-exp-part 'apply-action5 .02 (get-disc (get-board (apply-action (init-state 4 6 YELLOW) 2)) (cons 2 0)) YELLOW)
(check-exp-part 'apply-actions1 .02 (get-player (apply-actions (init-state 7 6 YELLOW) '(1 0 2 1 1 3 4 1 2 3))) YELLOW)
(check-exp-part 'apply-actions2 .02 (get-disc (get-board (apply-actions (init-state 6 6 RED) '(0 1 1 0 1 1 0 2 3))) (cons 0 2)) RED)
(check-exp-part 'apply-actions3 .02 (get-available-actions (get-board (apply-actions (init-state 6 6 RED) '(0 1 1 0 1 1 0 2 3 1 1)))) '(0 2 3 4 5))
(check-exp-part 'apply-actions4 .02 (get-available-actions (get-board (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2 3 3)))) '(3))
(check-exp-part 'apply-actions5 .02 (get-available-actions (get-board (apply-actions state-test '(1 1 1 1 1 1 0 0 0 0 0 0)))) '(3 4 5 6))
(check-exp-part 'apply-actions6 .02 (get-available-actions (get-board (apply-actions (init-state 4 4 RED) '(2 0 0 0 1 1 3 0 2 2 1 1 3 3 3 2)))) '())
(check-exp-part 'apply-actions7 .02 (get-available-actions (get-board (apply-actions (init-state 7 9 YELLOW) '(5 5 8 5 5 8 8 0 0 4 1 5 5 6 1 1 7 8 2 3 1 5 3 6 1 0 3 1 1 0 4 3 2)))) '(0 2 3 4 6 7 8))
(check-exp-part 'apply-actions8 .02 (get-available-actions (get-board (apply-actions (init-state 12 12 RED) '(9 6 8 10 3 1 1 3 7 5 11 11 7 3 11 0 5 6 7 9 5 3 0 10 5 10 10 6 1 7 0 3)))) '(0 1 2 3 4 5 6 7 8 9 10 11))
(check-exp-part 'apply-actions9 .02 (get-available-actions (get-board (apply-actions (init-state 15 15 YELLOW) '(8 10 0 13 9 2 9 6 1 5 14 6 3 3 11 5 13 7 13 13 3 13 10 8 9 11 1 12 12 6 4 5 2 12)))) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(check-exp-part 'apply-actions10 .02 (get-available-actions (get-board (apply-actions (init-state 8 15 RED) '(6 1 3 3 10 12 14 1 8 3 9 10 3 11 3 9 7)))) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(check-exp-part 'is-game-over?1 .01 (is-game-over? (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2 3))) RED)  
(check-exp-part 'is-game-over?2 .01 (is-game-over? (apply-actions (init-state 5 7 RED) '(0 3 1 3 2 3 4 4 5 0 1 5 0 6 6 2))) YELLOW)  
(check-exp-part 'is-game-over?3 .01 (is-game-over? (apply-actions (init-state 5 6 YELLOW) '(0 1 2 3 4 5 5 4 3 2 1 0 0 1 3 2 3 2 0 3))) RED)  
(check-exp-part 'is-game-over?4 .01 (is-game-over? (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2))) #f)
(check-exp-part 'is-game-over?5 .01 (is-game-over? (apply-actions (init-state 6 6 YELLOW) '(0 5 0 5 0 3 0))) YELLOW)
(check-exp-part 'is-game-over?6 .01 (is-game-over? state-test) #f)
(check-exp-part 'is-game-over?7 .02 (is-game-over? (apply-actions (init-state 4 4 RED) '(2 0 0 0 1 1 3 0 2 2 1 1 3 3 3 2))) 3)
(check-exp-part 'is-game-over?8 .02 (is-game-over? (apply-actions (init-state 8 15 RED) '(6 1 3 3 10 12 14 1 8 3 9 10 3 11 3 9 7))) RED)
(check-exp-part 'is-game-over?9 .02 (is-game-over? (apply-actions (init-state 8 15 YELLOW) '(6 8 5 2 1 4 9 7 12 9 12 9 8 3 3 10 6 7 11 6 12 13 9 6 0 6 10 7 1 10 7 12 13 14 8 11 7 7 7 5 7 9 2 11 1 3 11 12 3 12 11 9 11 1 8 9 12 14 5 3 2))) YELLOW)
(check-exp-part 'is-game-over?10 .02 (is-game-over? (apply-actions (init-state 8 8 RED) '(2 0 0 1 7 0 1 4 7 2 1 3 6 7 7 6 3 3 3 7 7 2 0 4 4 3 7 4))) YELLOW)
(check-exp-part 'is-game-over?11 .02 (is-game-over? (apply-actions (init-state 4 20 RED) '(16 13 8 13 17 14 5 0 15 13 18))) RED)
(check-exp-part 'is-game-over?12 .02 (is-game-over? (apply-actions (init-state 7 7 RED) '(4 4 0 1 1 4 1 1 1 5 3 1 4 6 4 2 0 5 6 3 6 5 0 6 3 1 5 3))) YELLOW)
(check-exp-part 'is-game-over?13 .02 (is-game-over? (apply-actions (init-state 5 8 RED) '(5 1 0 0 1 6 7 4 7 2 4 2 3 3 4 4 6 2 2 0 1 6 4 3))) YELLOW)
(check-exp-part 'is-game-over?14 .02 (is-game-over? (apply-actions (init-state 4 8 RED) '(0 0 0 0 1 1 1 1 2 3 2 2 2 3 4 3 3 4 4 4 5 5 5 5 7 6 6 6 6 7 7 7))) 3)
(check-exp-part 'is-game-over?15 .02 (is-game-over? (apply-actions (init-state 4 8 RED) '(1 0 5 3 7 0 0 1 6 0 7 1 7 6 5 2))) #f)
(check-exp-part 'is-game-over?16 .02 (is-game-over? (apply-actions (init-state 9 4 YELLOW) '(0 2 1 1 3 3 1 0 2 1 2 2 3 3 1 0 2 3 2 3 0))) YELLOW)
(check-exp-part 'is-game-over?17 .02 (is-game-over? (apply-actions (init-state 9 4 RED) '(1 1 1 1 3 0 0 2 1 1 2 2 0 2 1 1 3 2))) #f)
(check-exp-part 'is-game-over?18 .02 (is-game-over? (apply-actions (init-state 5 5 RED) '(4 3 2 4 3 2 3 2 0 1 1 0 4 2 3 4 2 1 4 0 1 3))) 2)
(check-exp-part 'is-game-over?19 .02 (is-game-over? (apply-actions (init-state 7 4 RED) '(2 0 0 2 1 3 1 0 3 0 2 0 0 0 2 1))) 2)
(check-exp-part 'is-game-over?20 .02 (is-game-over? (apply-actions (init-state 7 9 YELLOW) '(1 6 5 2 0 3 2 2 6 4 3 1 1 0 1 5 8 5 5 7 2 8 2 1 1 2 5 0 5 6 6 8 4 7 2 0 1 8 7 7))) 1)
(check-exp-part 'is-game-over?21 .02 (is-game-over? (apply-actions (init-state 5 9 YELLOW) '(8 0 7 6 6 0 1 5 6 0 7 5 4 8 5 6 0 0 5))) 2)
(check-exp-part 'is-game-over?22 .02 (is-game-over? (apply-actions (init-state 10 4 YELLOW) '(2 1 2 0 3 1 3 0 0 3 0 2 1))) 2)
(check-exp-part 'is-game-over?23 .02 (is-game-over? (apply-actions (init-state 10 6 RED) '(0 0 4 1 2 5 0 0 1 4 0 1 1 3 2 0 4 1 2 2 0 1 1 5 1 1 1 5 2 2 5 5 3))) 1)
(check-exp-part 'is-game-over?24 .02 (is-game-over? (apply-actions (init-state 10 4 YELLOW) '(2 1 2 0 3 1 3 0 0 3 0 2 1))) 2)
(check-exp-part 'is-game-over?25 .02 (is-game-over? (apply-actions (init-state 10 7 YELLOW) '(3 1 3 1 4 5 2 0 1 0 2 0 2 6 4 2 1))) 2)
(check-exp-part 'is-game-over?26 .02 (is-game-over? (apply-actions (init-state 10 10 YELLOW) '(2 9 3 5 7 2 6 1 0 4 8 7 2 5 0 9 5 5 7 1 6 7 7 6))) 1)
(check-exp-part 'is-game-over?27 .02  (is-game-over? (apply-actions (init-state 6 4 RED) '(0 1 2 3 3 2 1 0 0 1 2 3 0 1 3 2))) 1)
(check-exp-part 'is-game-over?28 .02 (is-game-over? (apply-actions (init-state 5 6 RED) '(0 1 2 3 4 5 5 4 2 3 1 0 0 1 3 2 3 4 4 5 0 5 5))) RED)
;; SFÂRȘIT CHECK - TASK 1 - NU modificați această linie!
;;
;; CHECK - Task2 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 2 : 30 puncte) ;;check-exp
(define FUNCTIONS (list is-game-over? play-game get-available-actions apply-actions)) ;;check-exp
(check-in-part 'get-action1 .1 (get-action (apply-actions (init-state 9 4 YELLOW) '(0 0 3 3 3 2 2 1 2 2 3 1 0 1 0))) '(1 3))
(check-in-part 'get-action2 .1 (get-action (apply-actions (init-state 7 7 RED) '(1 4 2 4 6 2 1 4 1 0 5 3 6 0 5 1 5 0 4 2 1 5))) '(0 3))
(check-exp-part 'get-action3 .1 (get-action (apply-actions (init-state 10 6 YELLOW) '(3 4 3 3 4 5 4 2 3 2 3 3 2 4 2 3 3 2 4 4 0 0 3 4 1 0 3 1 0 1 5 2))) 1)
(check-exp-part 'get-action4 .05 (get-action (apply-actions (init-state 4 4 YELLOW) '(1 0 1 1 0 3 0 0 3 2 3 3))) 2)
(check-exp-part 'get-action5 .05 (get-action (apply-actions (init-state 4 4 RED) '(0 3 2 3 3 1 1 2 2))) 3)
(check-exp-part 'get-action6 .05 (get-action (apply-actions (init-state 8 8 RED) '(3 0 2 1 6 6 5 1))) 4)
(check-exp-part 'get-action7 .1 (get-action (apply-actions (init-state 10 5 YELLOW) '(1 2 4 1 3 0 3 2 2 1 2 1 2))) 1)
(check-exp-part 'get-action8 .1 (get-action (apply-actions (init-state 12 12 YELLOW) '(6 3 0 9 4 2 10 7 1 7 0 0 9 2 0 8 2 8 8 10 10 10 5 2 3 11 4 4 4 8 3 2 2 11 11 8 8))) 9)
(check-in-part 'get-action9 .1 (get-action (apply-actions (init-state 20 8 RED) '(3 2 2 1 2 7 2 5 0 6 1 5 0 5))) '(2 3))
(check-in-part 'get-action10 .1 (get-action (apply-actions (init-state 10 10 YELLOW) '(0 4 0 5 0 6 1 7 1 8 1 9 2 9 2 8 2))) '(0 1 2 3 4 5 6 7 8 9))
(check-exp-part 'play-game1 .025 (check-game AI (init-state 8 9 RED) (cons select-random-action (current-pseudo-random-generator)) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 1) #t)
(check-exp-part 'play-game2 .025 (check-game AI (init-state 9 8 YELLOW) (cons simple-strategy 'None) (cons  simple-strategy 'None) FUNCTIONS 1) #t) 
(check-exp-part 'play-game3 .025 (check-play-game AI (init-state 7 7 YELLOW) (cons simple-strategy 'None) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 4 YELLOW #f) #t) 
(check-exp-part 'play-game4 .025 (check-game AI state-test (cons select-random-action (current-pseudo-random-generator)) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 1) #t) 
(check-exp-part 'play-game5 .025 (check-play-game AI (init-state 10 5 YELLOW) (cons simple-strategy 'None) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 4 YELLOW #f) #t) 
(check-exp-part 'play-game6 .025 (check-play-game AI (init-state 10 10 YELLOW) (cons simple-strategy 'None) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 4 YELLOW #f) #t) 
;; SFÂRȘIT CHECK - TASK 2 - NU modificați această linie!
;;
;; CHECK - BONUS - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Bonus 3 : 20 puncte BONUS) ;;check-exp
(check-exp-part 'bonus1 .1 (check-play-game AI state-test (cons negamax 1) (cons negamax 3) FUNCTIONS 4 RED #t) #t)
(check-exp-part 'bonus2 .1 (check-play-game AI state-test (cons select-random-action (current-pseudo-random-generator)) (cons negamax 4) FUNCTIONS 4 RED #t) #t)
(check-exp-part 'bonus3 .1 (check-play-game AI (apply-actions (init-state 4 6 RED) '(1 0 1 1 0 3)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 5) FUNCTIONS 6 YELLOW #t) #t)
(check-exp-part 'bonus4 .1 (check-play-game AI (init-state 7 7 YELLOW) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 6 RED #t) #t)
(check-exp-part 'bonus5 .1 (check-play-game AI (apply-actions (init-state 9 6 YELLOW) '(1 2 0 3 1 0 2 4 5 0)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 6 RED #t) #t)
(check-exp-part 'bonus6 .1 (check-play-game AI (init-state 8 8 RED) (cons negamax 1) (cons negamax 3) FUNCTIONS 6 YELLOW #t) #t)
(check-exp-part 'bonus7 .1 (check-play-game AI (init-state 4 6 RED) (cons negamax 1) (cons negamax 5) FUNCTIONS 4 YELLOW #t) #t)
(check-exp-part 'bonus8 .1 (check-play-game AI (init-state 5 7 RED) (cons negamax 2) (cons negamax 5) FUNCTIONS 2 YELLOW #t) #t)
(check-exp-part 'bonus9 .1 (check-play-game AI (apply-actions (init-state 10 8 RED) '(1 5 2 6 7 1 0 2 6 4 0 7 7)) (cons negamax 2) (cons negamax 4) FUNCTIONS 4 YELLOW #t) #t)
(check-exp-part 'bonus10 .1 (check-play-game AI (apply-actions (init-state 10 6 RED) '(1 0 1 1 0 3)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 4 YELLOW #t) #t)
;; SFÂRȘIT CHECK - BONUS - NU modificați această linie!
;; SFÂRȘIT SECȚIUNE DE TESTARE
(sumar)
