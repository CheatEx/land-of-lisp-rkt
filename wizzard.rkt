#lang racket
(define *nodes*
  '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden. there is a well in front of you.))
    (attic (you are in the attic. there is a giant welding torch in the corner.))))
(define *edges* '((living-room (garden west door) (attic upstairs ladder))
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))
(define *objects* '(whiskey bucket frog chain))

(define *location* 'living-room)
(define *object-locations* '((whiskey living-room)
                             (bucket living-room)
                             (chain garden)
                             (frog garden)))

(define (describe-location location nodes)
  (cadr (assoc location nodes)))

(define (describe-paths location all-edges)
  (define (describe-path edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))
  (let* ((edges (cdr (assoc location all-edges)))
         (descriptions (map describe-path edges)))
    (apply append descriptions)))

(define (describe-objects loc objs obj-locs)
  (define (objects-at loc objs obj-locs)
    (define (there? obj)
      (eq? (cadr (assoc obj obj-locs)) loc))
    (filter there? objs))
  (define (describe-object obj)
    `(you see a ,obj on the floor.))
  (let* ((objs-here (objects-at loc objs obj-locs))
         (descriptions (map describe-object objs-here)))
    (apply append descriptions)))

(define (look)
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))


