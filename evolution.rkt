#lang racket

(require racket/set)

(define *width* 120)
(define *height* 30)
(define *jungle* '(45 10 10 10))
(define *plant-energy* 60)
(define *reproduction-energy* 200)
(define *plants* (set))

(struct animal (x y energy dir genes) #:transparent #:mutable)

(define *animals* (list
                   (animal
                    (truncate (/ *width* 2))
                    (truncate (/ *height* 2))
                    150
                    0
                    (for/list ([i (in-range 8)])
                      (add1 (random 10))))))

(define (random-plant! left top width height)
  (let ([pos (cons
              (+ left (random width))
              (+ top (random height)))])
    (set! *plants* (set-add *plants* pos))))

(define (add-plants!)
  (apply random-plant! *jungle*)
  (random-plant! 0 0 *width* *height*))

(define (move! animal)
  (let* ([dir (animal-dir animal)]
         [x-shift (cond [(and (>= dir 2) (< dir 5)) 1]
                        [(or (= dir 1) (= dir 5)) 0]
                        [else -1])]
         [y-shift (cond [(and (>= dir 0) (< dir 3)) -1]
                        [(and (>= dir 4) (< dir 7)) 1]
                        [else 0])])
    (set-animal-x! animal
                   (remainder (+ (animal-x animal) x-shift *width*)
                              *width*))
    (set-animal-y! animal 
                   (remainder (+ (animal-y animal) y-shift *height*)
                              *height*))
    (set-animal-energy! animal (sub1 (animal-energy animal)))))

(define (angle genes)
  (let* ([x (random (apply + genes))]
         [xnu (- x (car genes))])
    (if (< xnu 0)
        0
        (add1 (angle (cdr genes))))))

(define (turn! animal)
  (set-animal-dir! animal
                   (remainder (+ (animal-dir animal) (angle (animal-genes animal)))
                              8)))

(define (eat! animal)
  (let ([pos (cons (animal-x animal) (animal-y animal))])
    (when (set-member? *plants* pos)
      (set-animal-energy! animal (+ (animal-energy animal) *plant-energy*))
      (set! *plants* (set-remove *plants* pos)))))

(define (mutate genes mutation)
  (if (null? genes)
      null
      (if (equal? mutation 0)
          (cons (max 1 (+ (car genes) (random 3) -1))
                (mutate (cdr genes) (sub1 mutation)))
          (cons (car genes)
                (mutate (cdr genes) (sub1 mutation))))))

(define (reproduce! a)
  (let ([e (animal-energy a)])
    (when (>= e *reproduction-energy*)
      (set-animal-energy! a (truncate (/ e 2)))
      (let* ([mutation (random 8)]
             [new-animal (struct-copy animal a
                                      [genes (mutate (animal-genes a) mutation)])])
        (set! *animals* (cons new-animal *animals*))))))

(define (day!)
  (set! *animals*
        (filter (lambda (a) (>= (animal-energy a) 0))
                *animals*))
  (for-each (lambda (a)
              (turn! a)
              (move! a)
              (eat! a)
              (reproduce! a))
            *animals*)
  (add-plants!))

(define (draw-world)
  (for* ([y (in-range *height*)]
         [x (in-range *width*)])
    (when (= x 0)
      (display #"|"))
    (when (= x (sub1 *width*))
      (display #"|") (display #\newline))
    (display (cond
               [(for/or ([a *animals*]) (and (= (animal-x a) x) (= (animal-y a) y)))
                #"M"]
               [(set-member? *plants* (cons x y)) #"*"]
               [else #\space]))))

(define (evolution)
  (draw-world)
  (display #\newline)
  (let ([str (read-line)])
    (cond [(equal? str "quit") null]
          [else (let ([turns (string->number str)])
                  (if turns
                      (for ([i (in-range (add1 turns))])
                        (day!))
                      (day!))
                  (evolution))])))
