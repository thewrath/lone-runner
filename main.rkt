#lang racket

(require r-cade)

(define DEBUG #f)

(define M-WIDTH 256)
(define M-HEIGHT 256)

(define B-SIZE 8)
(define HALF-B-SIZE (/ B-SIZE 2))

(define P-SPEED 1)
(define GRAVITY 1)

(define UP 0)
(define RIGHT 1)
(define DOWN 2)
(define LEFT 3)

(define debug-text "debug")

; Levels definitons
(define asset/levels #(
                       #(
                         #("w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "l" "w" "w" "w" "w" "w" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w")
                         )))

(define asset/sprites (hash
                        "p" '(
                              #b00111100
                              #b00011000
                              #b00011000
                              #b01111110
                              #b01000010
                              #b01000010
                              #b01000010)
                        "l" '(
                              #b01000010
                              #b01000010
                              #b01000010
                              #b01111110
                              #b01000010
                              #b01000010
                              #b01000010)
                        "db" '(
                              #b11111111
                              #b10000001
                              #b10010001
                              #b10111001
                              #b10010001
                              #b10000001
                              #b11111111)
                        "w" '(
                              #b01111110
                              #b10000001	
                              #b10000001
                              #b10000001
                              #b10000001
                              #b10000001
                              #b01111110)))

; Define all primitive of our game
(struct point (x y) #:transparent #:mutable)
(struct block (position coords type) #:transparent #:mutable)
(struct player (position block) #:transparent #:mutable)
(struct enemy (position) #:transparent)
(struct level (indice gold) #:transparent)
(struct world (player enemies level) #:transparent)

(define (point-equals? a b) 
  (and 
    (= (point-x a) (point-x b)) 
   	(= (point-y a) (point-y b))))

(define (point-add a b)
  (point (+ (point-x a) (point-x b)) (+ (point-y a) (point-y b))))

;; UPDATE
; Player actions
(define (actions/up) (action btn-up #t))
(define (actions/right) (action btn-right #t))
(define (actions/down) (action btn-down #t))
(define (actions/left) (action btn-left #t))
(define (actions/display-current-block) (action btn-z))

(define (update/world w) 
  (define e (world-enemies w))
  (define l (world-level w))
  (define p (update/player (world-player w) (level-indice l)))
  (begin 
    (when ((actions/display-current-block))
      (set! debug-text (get-block (level-indice l) (player-position p) #:mode "upper")))
    (world p e l)))

(define (update/player p l)
  (define new-player (apply-gravity (move-player p l) l))
  new-player)

(define (move-player p l)
  (let ([x (point-x (player-position p))]
        [y (point-y (player-position p))]
        [dx 0]
        [dy 0])
    (when (and ((actions/up)) (can-player-go? p l UP))
      (set! dy -1))
    (when (and ((actions/right)) (can-player-go? p l RIGHT))
      (set! dx 1))
    (when (and ((actions/down)) (can-player-go? p l DOWN))
      (set! dy 1))
    (when (and ((actions/left)) (can-player-go? p l LEFT))
      (set! dx -1))
    (player (point
              (+ x (* P-SPEED dx))
              (+ y (* P-SPEED dy)))(get-block l (player-position p) #:mode "upper"))))

(define (can-player-go? p l dir)
  (let* ([blocks (get-blocks-around-player p l)]
         [block (list-ref blocks dir)]) 
    (and 
      (not (null? block)) 
      (not (equal? "w" (block-type block))))))

; Apply gravity to a player/enemy
(define (apply-gravity p l)
  (let* ([position (player-position p)]
        [x (point-x position)]
        [y (point-y position)]
        [b (player-block p)])
    (if (and (can-player-go? p l DOWN) (not (equal? "l" (block-type b))))
        (player (point x (+ y GRAVITY)) b)
        p)))

;; DRAW
; Little helper to draw color palette 
(define (debug/draw-palette)
  (for ([i (range 16)])
    (color i)
    (draw 0 i '(#xff))))

(define (draw/world w)
  (draw/level (level-indice (world-level w)))
  (draw/player (world-player w) (level-indice (world-level w))))

; Draw a specific level (l correspond to level indice in asset/levels)
(define (draw/level i)
  (define (draw-block b)
    (let ([x (point-x (block-position b))]
          [y (point-y (block-position b))]
          [t (block-type b)])
      (when (hash-has-key? asset/sprites t)
        (draw x y (hash-ref asset/sprites t)))))
  (on-level-map i draw-block))

; Draw player
(define (draw/player p l)
  (let* ([x (point-x (player-position p))]
         [y (point-y (player-position p))]
         [debug-blocks (cons (player-block p) (get-blocks-around-player p l))])
    (draw x y (hash-ref asset/sprites "p"))
    (color 3)
    (and DEBUG (map (lambda (b)
                      (unless (null? b)
                        (let ([bx (point-x (block-position b))]
                            [by (point-y (block-position b))])
                        (draw bx by (hash-ref asset/sprites "db"))))
                      ) debug-blocks))))

;; HELPERS
(define (on-level-map i f)
  (let ([x 0] [y 0])
    (for ([l (vector-ref asset/levels i)])
      (set! x 0)
      (for ([t l])
        (f (block (point (* x B-SIZE) (* y B-SIZE)) (point x y) t))
        (set! x (add1 x)))
      (set! y (add1 y)))))

(define (on-level-row i f)
  (let ([x 0])
    (for ([c (vector-ref (vector-ref asset/levels i) 0)])
      (f x c)
      (set! x (+ x B-SIZE)))))

;; Return block under a given point for a given level
(define (get-block l p #:mode mode)
  (define operator (if (equal? mode "upper") <= >=))
  (define b null)
  (let ([x (point-x p)]
        [y (point-y p)])
    (define (is-nearest? bx by)
      (and (operator (abs (- bx x)) (/ B-SIZE 2))
           (operator (abs (- by y)) (/ B-SIZE 2))))
    (on-level-map l 
                  (lambda (current-block) 
                    (when (is-nearest? (point-x (block-position current-block)) (point-y (block-position current-block)))
                      (set! b current-block)))))
  b)

; Get direction from cb block to db block
(define (get-block-direction cb db)
  (let* ([cb-pos (block-position cb)]
        [db-pos (block-position db)]
        [dir (point 
               (- (point-x cb-pos) (point-x db-pos))
               (- (point-y cb-pos) (point-y db-pos)))])
    dir))

(define (get-blocks-around-player p l)
  (if (null? (player-block p))
      (list)
      (let* ([pb (player-block p)]
             [pbc (point-x (block-coords pb))]
             [pbl (point-y (block-coords pb))])
      (map (lambda (b) (if (block-in-map? b l) 
                           (begin (set-block-type! b (get-block-type b l)) b) 
                           null)) ; This little lamba fill block type
           (list
              (block (point (* pbc B-SIZE) (* (sub1 pbl) B-SIZE)) (point pbc (sub1 pbl)) "e") ; up
              (block (point (* (add1 pbc) B-SIZE) (* pbl B-SIZE)) (point (add1 pbc) pbl) "e") ; right
              (block (point (* pbc B-SIZE) (* (add1 pbl) B-SIZE)) (point pbc (add1 pbl)) "e") ; down
              (block (point (* (sub1 pbc) B-SIZE) (* pbl B-SIZE)) (point (sub1 pbc) pbl) "e")))))) ; left

; Check if block coords are in bounds of map 
(define (block-in-map? b l)
  (let ([bc (point-x (block-coords b))]
        [bl (point-y (block-coords b))]
        [level (vector-ref asset/levels l)])
    (and
      (and (< bl (vector-length level)) (>= bl 0))
      (and (< bc (vector-length (vector-ref level bl))) (>= bc 0)))))

(define (get-block-type b l) 
  (let ([bc (point-x (block-coords b))]
        [bl (point-y (block-coords b))]
        [level (vector-ref asset/levels l)]) 
    (vector-ref (vector-ref level bl) bc)))

;; INIT
(define (init)
  (define p (player (point 50 50) null))
  (define e (list (enemy (point 10 10))))
  (define l (level 0 100))
  (world p e l))

(define my-world (init))

(define (game-loop)
  (cls)
  (color 1)
  (set! my-world (update/world my-world))
  (draw/world my-world)
  (color 3)
  (and DEBUG (text 0 50 debug-text)))
;(draw 50 0 (hash-ref asset/sprites "player"))

(run game-loop M-WIDTH M-HEIGHT #:scale 2 #:shader #f)