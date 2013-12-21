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

(define *player-pos* null)

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

(define (add-cops edge-alist edges-with-cops)
  (map (lambda (edge-entry)
         (let ([node1 (car edge-entry)]
               [node1-edges (cdr edge-entry)])
           (cons node1
                 (map (lambda (edge)
                        (let* ([node2 (car edge)]
                               [edges-set (list->set (edge-pair node1 node2))]
                               [cop-edges-set (list->set edges-with-cops)])
                          (if (not (empty? (set-intersect edges-set cop-edges-set)))
                              (list node2 'cops)
                              edge)))
                      node1-edges))))
       edge-alist))

(define (neighbors node edge-alist)
  (map car (cdr (assoc node edge-alist))))

(define (within-one a b edge-alist)
  (member b (neighbors a edge-alist)))

(define (within-two a b edge-alist)
  (or (within-one a b edge-alist)
      (for/or ((i (neighbors a edge-alist)))
        (within-one i b edge-alist))
      ))

(define (make-city-nodes edge-alist)
  (let ([wumpus (random-node)]
        [glow-worms (map (lambda (x) (random-node)) (in-range 0 *worm-num*))])
    (map
     (lambda (n)
       (append (list n)
               (cond ((equal? n wumpus) '(wumpus))
                     ((within-two n wumpus edge-alist) '(blood))
                     (else '()))
               (cond ((member n glow-worms) '(glow-worm))
                     ((for/or ((worm glow-worms)) (within-one n worm edge-alist))
                      '(lights))
                     (else '()))
               (cond ((for/or ((near-edge (assoc n edge-alist))) (not (null? (cdr near-edge)))) '(sirens))
                     (else '())))
       (in-range 1 (+ *node-num* 1))))))

(define (find-empty-node)
  (let ((x (random-node)))
    (if (not (empty? (cdr (assoc x *congestion-city-nodes*))))
        (find-empty-node)
        x)))

(define (new-game)
  (set! *congestion-city-edges* (make-city-edges))
  (set! *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (set! *player-pos* (find-empty-node))
  (set! *visited-nodes* (list *player-pos*)))
