#lang racket

(require (for-syntax syntax/parse))

(define *nodes*
  '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden. there is a well in front of you.))
    (attic (you are in the attic. there is a giant welding torch in the corner.))))

(struct edge (destination direction path))

(define *edges* `((living-room ,(edge 'garden 'west 'door) ,(edge 'attic 'upstairs 'ladder))
                  (garden ,(edge 'living-room 'east 'door))
                  (attic ,(edge 'living-room 'downstairs 'ladder))))

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
    `(there is a ,(edge-path edge) going ,(edge-direction edge) from here.))
  (let* ((edges (cdr (assoc location all-edges)))
         (descriptions (map describe-path edges)))
    (apply append descriptions)))

; TODO use locations only
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
  (let ([next (findf
               (lambda (edge) (eq? direction (edge-direction edge)))
               (cdr (assoc *location* *edges*)))])
    (if next
        (begin
          (set! *location* (edge-destination next))
          (look))
        '(you cant go there)
        )))

(define (pickup object)
  (if (member object (objects-at *location* *objects* *object-locations*))
      (set! *object-locations* (cons (list object 'body) *object-locations*))
      '(you cant get that)))

(define (inventory)
  (objects-at 'body *objects* *object-locations*))

(define (have? object)
  (member object (inventory)))

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

(define-syntax (action stx)
  (syntax-parse stx
    [(_ cmd:id subj:expr obj:expr place:expr body:expr ...+)
     #:fail-when (identifier-binding #'cmd)
                 (format "command '~a' already defined" (syntax->datum #'cmd))
     #'(begin
         (define (cmd subject object)
           (if (and (eq? *location* place)
                    (eq? subject subj)
                    (eq? object obj)
                    (have? subj))
               (begin body ...)
               '(cant cmd like that.)))
          (set! *commands* (cons (quote cmd) *commands*)))]
    [(_ cmd:id subj:expr obj:expr place:expr)
     #:fail-when #t "missing action body"
     #'()]))

(define *chain-welded* #f)

(action weld 'chain 'bucket 'attic
        (if (and (have? 'bucket)
                 (not *chain-welded*))
            (begin (set! *chain-welded* #t)
                   '(the chain is now securely welded to the bucket.))
            '(you do not have a bucket.)))

(define *bucket-filled* #f)

(action dunk 'bucket 'well 'garden
        (if *chain-welded*
            (begin (set! *bucket-filled* #t)
                   '(the bucket is now full of water))
            '(the water level is too low to reach.)))

(action splash 'bucket 'wizard 'living-room
        (cond [(not *bucket-filled*) '(the bucket has nothing in it.)]
              [(have? 'frog) '(the wizard awakens and sees that you stole his frog.
                                   he is so upset he banishes you to the
                                   netherworlds- you lose! the end.)]
              [else '(the wizard awakens from his slumber and greets you warmly.
                          he hands you the magic low-carb donut- you win! the end.)]))
