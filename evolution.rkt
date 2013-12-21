#lang racket

(require racket/set)

(define *width* 100)
(define *height* 30)
(define *jungle* '(45 10 10 10))
(define *plant-energy* 80)
(define *plants* (set))

(struct animal (x y energy dir genes) #:transparent)

(define *animals* (list
                   (animal
                    (truncate (/ *width* 2))
                    (truncate (/ *height* 2))
                    1000
                    0
                    (for/vector ([i (in-range 8)])
                             (add1 (random 10))))))

(define (random-plant! left top width height)
  (let ([pos (cons
              (+ left (random width))
              (+ top (random height)))])
    (set! *plants* (set-add *plants* pos))))

(define (add-plants!)
  (apply random-plant! *jungle*)
  (random-plant! 0 0 *width* *height*))

