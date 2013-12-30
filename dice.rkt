#lang racket

(define *num-players* 2)
(define *max-dice* 3)
(define *board-size* 2)
(define *board-hex-num* (* *board-size* *board-size*))

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
        
    