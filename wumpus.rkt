#lang racket

(require "graph-util.rkt")

(define *congestion-city-nodes* null)
(define *congestion-city-edges* null)
(define *visited-nodes* null)
(define *node-num* 30)
(define *edge-num* 45)
(define *worm-num* 3)
(define *cop-odds* 15)

(define (random-node)
  (+ 1 (random *node-num*)))

(define (edge-pair a b)
  (if (equal? a b)
    null
    (list (cons a b) (cons b a))))

(define (make-edge-list)
  (define (edge-loop n)
    (if (> n 0)
      (append
        (edge-pair (random-node) (random-node))
        (edge-loop (- n 1)))
      null))
  (edge-loop *node-num*))

(define (direct-edges node edges)
  (filter
    (lambda (edge) (equal? (car edge) node))
    edges))

(define (get-connected node edges)
  (let ([visited null])
    (define (traverse node)
      (unless (member node visited)
        (set! visited (cons node visited))
        (map (lambda (edge) (traverse (cdr edge)))
          (direct-edges node edges))))
    visited))