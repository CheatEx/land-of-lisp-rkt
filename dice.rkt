#lang racket

(define *num-players* 2)
(define *max-dice* 3)
(define *board-size* 2)
(define *board-hex-num* (* *board-size* *board-size*))

(struct game (player-num board moves) #:transparent)

(define (gen-board)
  (for/vector ([i (in-range *board-hex-num*)])
    (list (random *num-players*)
          (add1 (random *max-dice*)))))

(define (player-letter player-num)
  (integer->char (+ 97 player-num)))

(define (hex board x y)
  (vector-ref board (+ x (* y *board-size*))))

(define (draw-board board)
  (for ([y (in-range *board-size*)])
    (display #\newline)
    (for ([i (in-range (- *board-size* y))])
      (display "  "))
    (for ([x (in-range *board-size*)])
      (let ([hex (hex board x y)])
        (display (player-letter (first hex)))
        (display #\-)
        (display (second hex))
        (display #\space)))))

(define (game-tree board player spare-dice first-move)
  (game player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

; TODO struct for hex and move
(define (add-passing-move board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list null
                  (game-tree (add-new-dice board player (sub1 spare-dice))
                             (remainder (add1 player) *num-players*)
                             0
                             #t))
            moves)))

(define (attacking-moves board cur-player spare-dice)
  (define (player pos)
    (car (vector-ref board pos)))
  (define (dice pos)
    (cadr (vector-ref board pos)))
  (for/list ([src (in-range *board-hex-num*)]
             #:when (equal? (player src) cur-player)
             [dst (neighbors src)]
             #:when (and (not (equal? (player dst) cur-player))
                         (> (dice src) (dice dst))))
    (list (list src dst)
          (game-tree (board-attack board cur-player src dst (dice src))
                     (remainder (add1 cur-player) *num-players*)
                     (+ spare-dice (dice dst))
                     #f))))

(define (neighbors pos)
  (let ([up (- pos *board-size*)]
        [down (+ pos *board-size*)])
    (for/list ([p (append (list up down)
                          (if (not (equal? (remainder pos *board-size*) 0))
                              (list (sub1 up) (sub1 pos))
                              null)
                          (if (not (equal? (remainder (add1 pos) *board-size*) 0))
                              (list (add1 pos) (add1 down))
                              null))]
               #:when (and (>= p 0) (< p *board-hex-num*)))
      p)))

(define (board-attack board player src dst dice)
  (for/vector ([(pos hex) (in-dict board)])
    (cond ((equal? pos src) (list player 1))
          ((equal? pos dst) (list player (sub1 dice)))
          (else hex))))

(define (add-new-dice board player spare-dice)
  (define (f lst n)
    (cond ((null? lst) null)
          ((equal? n 0) lst)
          (else (let ([cur-player (caar lst)]
                      [cur-dice (cadar lst)])
                  (if (and (equal? cur-player player) (< cur-dice *max-dice*))
                      (cons (list cur-player (add1 cur-dice))
                            (f (cdr lst) (sub1 n)))
                      (cons (car lst) (f (cdr lst) n)))))))
  (list->vector(f (vector->list board) spare-dice)))

(define (play-vs-human game)
  (print-info game)
  (if (not (null? (game-moves game)))
      (play-vs-human (handle-human game))
      (announce-winner (game-board game))))

(define (print-info tree)
  (display #\newline)
  (display "current player ")
  (display (player-letter (game-player-num tree)))
  (draw-board (game-board tree)))

(define (handle-human game)
  (display #\newline)
  (display "Choose move: ")
  (for ([move (game-moves game)]
        [i (in-naturals 1)])
    (let ([action (car move)])
      (printf "~n ~a: " i)
      (if (null? action)
          (display "End turn.")
          (printf "~a -> ~a" (car action) (cadr action)))))
  (display #\newline)
  (cadr (list-ref (game-moves game) (sub1 (read)))))

(define (announce-winner board)
  (display #\newline)
  (let ([w (winners board)])
    (if (> (length w) 1)
        (printf "The game is a tie between ~a" (map player-letter w))
        (printf "The winner is ~a" (player-letter (car w))))))

(define (winners board)
  (let* ([tally (for/list ([hex board]) (car hex))]
         [totals (map (lambda (player)
                        (cons player (count (lambda (t) (equal? t player)) tally)))
                      (remove-duplicates tally))]
         [best (apply max (map cdr totals))])
    (map car
         (filter (lambda (x)
                   (equal? (cdr x) best))
                 totals))))
