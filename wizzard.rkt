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

(define *commands* '(look walk pickup inventory))

(define (describe-location location nodes)
  (cadr (assoc location nodes)))

(define (describe-paths location all-edges)
  (define (describe-path edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))
  (let* ((edges (cdr (assoc location all-edges)))
         (descriptions (map describe-path edges)))
    (apply append descriptions)))

(define (objects-at loc objs obj-locs)
    (define (there? obj)
      (eq? (cadr (assoc obj obj-locs)) loc))
    (filter there? objs))

(define (describe-objects loc objs obj-locs)
  (define (describe-object obj)
    `(you see a ,obj on the floor.))
  (let* ((objs-here (objects-at loc objs obj-locs))
         (descriptions (map describe-object objs-here)))
    (apply append descriptions)))

(define (look)
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(define (walk direction)
  (let ((next (findf
               (lambda (edge) (eq? direction (cadr edge)))
               (cdr (assoc *location* *edges*)))))
    (if next
        (begin
          (set! *location* (car next))
          (look))
        '(you cant go there)
    )))

(define (pickup object)
  (if (member object (objects-at *location* *objects* *object-locations*))
      (set! *object-locations* (cons (list object 'body) *object-locations*))
      '(you cant get that)))

(define (inventory)
  (objects-at 'body *objects* *object-locations*))

(define (say-hello)
  (display "Please enter your name")
  (let ([name (read-line)])
    (display "Nice to meet you, ")
    (display name)))

(define (game-read)
  (let* ([str (open-input-string (string-append "(" (read-line) ")"))]
         [cmd (read str)])
    (define (quote-it s)
      (list 'quote s))
    (cons (car cmd) (map quote-it (cdr cmd)))))

(define (tweak-text lst caps lit)
  (if (not (null? lst))
    (let ([item (car lst)]
          [rest (cdr lst)])
      (cond
        [(eq? item #\space) (cons item (tweak-text rest caps lit))]
        [(member item '(#\! #\? #\.)) (cons item (tweak-text rest #t lit))]
        [(eq? item #\") (tweak-text rest caps (not lit))]
        [lit (cons item (tweak-text rest #f lit))]
        [caps (cons (char-upcase item) (tweak-text rest #f lit))]
        [#t (cons (char-downcase item) (tweak-text rest #f #f))]))
    null))

(define (write-to-string v)
  (let ([p (open-output-string)])
    (display v p)
    (get-output-string p)))

(define (trim str)
  (regexp-replace* #px"^(\\(|\\)|\\s)+|(\\(|\\)|\\s)+$" str ""))

(define (game-print lst)
  (let* ([chars (string->list (trim (write-to-string lst)))]
         [tweaked-chars (tweak-text chars #t #f)])
    (display (list->string tweaked-chars))
    (display #\newline)))
    

(define (game-eval expr)
  (if (member (car expr) *commands*)
      (eval expr)
      'unknown-command))

(define (game-repl)
  (let ([cmd (game-read)])
    (unless (eq? cmd 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
