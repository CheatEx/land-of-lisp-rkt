#lang racket

(require "graph-util.rkt")
(require racket/set)
(require racket/sequence)

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
    (traverse node)
    visited))

; TODO Handle sequence nodes and edges
(define (find-islands nodes edges)
  (let ([islands null]
        [nodes-set (list->set nodes)])
    (define (find-island nodes-set)
      (let* ([connected-set (list->set (get-connected (sequence-ref nodes-set 0) edges))]
             [unconnected-set (set-subtract nodes-set connected-set)])
        (set! islands (cons (set->list connected-set) islands))
        (when (not (set-empty? unconnected-set))
          (find-island unconnected-set))))
    (find-island nodes-set)
    islands))

(define (connect-with-bridges islands)
  (if (not (empty? (cdr islands)))
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))
    null))

; TODO Handle sequence nodes and edges
(define (connect-all-islands nodes edges)
  (append (connect-with-bridges (find-islands nodes edges)) edges))

(define (make-city-edges)
  (let* ([nodes (sequence->list (in-range 1 (+ *node-num* 1)))]
         [edges (connect-all-islands nodes (make-edge-list))]
         [cops (filter (lambda (x) (equal? (random *cop-odds*) 0))
                  edges)])
    (add-cops (edges->alist edges) cops)))

(define (edges->alist edges)
  (define (assoc-entry node)
    (cons node
          (set-map (lambda (edge) (list (cdr edge)))
                   (list->set (direct-edges node edges)))))
  (let* ([nodes (list->set (map car edges))]
         [assoc-set (set-map assoc-entry nodes)])
    (set->list assoc-set)))

(define (add-cops a b)
  null)