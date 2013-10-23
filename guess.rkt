#lang racket

(define *small* 1)
(define *big* 100)

(define (guess-my-number)
  (quotient (+ *small* *big*) 2))

(define (smaller)
  (set! *big* (- (guess-my-number) 1))
  (guess-my-number))

(define (bigger)
  (set! *small* (+ (guess-my-number) 1))
  (guess-my-number))