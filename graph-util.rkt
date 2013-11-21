#lang racket

(require scheme/system)

(provide graph->dot)
(provide dot->png)

(define *max-label* 30)

(define (write-to-string v)
  (let ([p (open-output-string)])
    (display v p)
    (get-output-string p)))

(define (dot-name exp)
  (regexp-replace* #px"([^a-z0-9])" (write-to-string exp) "_"))

(define (dot-label exp)
  (let ([full-name (write-to-string exp)])
    (if (> (string-length full-name) *max-label*)
        (string-append (substring full-name 0 (- *max-label* 3)) "...")
        full-name)))

(define (nodes->dot nodes)
  (map (lambda (node)
         (display "\n")
         (display (dot-name (car node)))
         (display "[label=\"")
         (display (dot-label node))
         (display "\"];"))
       nodes))

(define (edges->dot edges)
  (map (lambda (node)
         (map (lambda (edge)
                (display "\n")
                (display (dot-name (car node)))
                (display " -> ")
                (display (dot-name (car edge)))
                (display "[label=\"")
                (display (dot-label (cdr edge)))
                (display "\"];"))
              (cdr node)))
       edges))

(define (graph->dot nodes edges)
  (display "digraph {")
  (nodes->dot nodes)
  (edges->dot edges)
  (display "\n}"))

(define (dot->png fname thunk)
  (with-output-to-file fname thunk #:mode 'text #:exists 'replace)
  (system (string-append "dot -Tpng -O " fname)))

(define (graph->png nodes edges fname)
  (dot->png fname
            (lambda () (graph->dot nodes edges))))
