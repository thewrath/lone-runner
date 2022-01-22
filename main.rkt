#lang racket

(require r-cade)

(define DEBUG #t)

(define M-WIDTH 256)
(define M-HEIGHT 256)

(define B-SIZE 8)
(define HALF-B-SIZE (/ B-SIZE 2))

(define P-SPEED 1)
(define GRAVITY 2)

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
(struct block (position type) #:transparent)
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
  (define new-player (apply-gravity (move-player p l)))
  (define wall (get-wall new-player))
  (if (not (null? wall))
    (let ([dir (get-block-direction (player-block p) (player-block new-player))])
      (set! debug-text dir)
      (player 
        (point-add (player-position new-player) dir) 
        (player-block new-player)))
    new-player))

(define (move-player p l)
  (define x (point-x (player-position p)))
  (define y (point-y (player-position p)))
  (define dx 0)
  (define dy 0)
  (when ((actions/up))
    (set! dy -1))
  (when ((actions/right))
    (set! dx 1))
  (when ((actions/down))
    (set! dy 1))
  (when ((actions/left))
    (set! dx -1))
  (player (point 
            (+ x (* P-SPEED dx))
            (+ y (* P-SPEED dy)))(get-block l (player-position p) #:mode "upper")))

; Apply gravity to a player/enemy
(define (apply-gravity p)
  (let* ([position (player-position p)]
        [x (point-x position)]
        [y (point-y position)]
        [b (player-block p)])
    (if (and (< y (- M-HEIGHT (* 2 B-SIZE))) (not (equal? "l" (block-type b))))
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
  (draw/player (world-player w)))

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
(define (draw/player p)
  (let ([x (point-x (player-position p))]
        [y (point-y (player-position p))])
    (draw x y (hash-ref asset/sprites "p"))))

;; HELPERS
(define (on-level-map i f)
  (let ([x 0] [y 0])
    (for ([l (vector-ref asset/levels i)])
      (set! x 0)
      (for ([t l])
        (f (block (point x y) t))
        (set! x (+ x B-SIZE)))
      (set! y (+ y B-SIZE)))))

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

(define (get-wall p)
  (define b (player-block p))
  (if (and (not (null? b)) (equal? "w" (block-type b)))
      b
      null))

; Get direction from cb block to db block
(define (get-block-direction cb db)
  (let ([cb-pos (block-position cb)]
        [db-pos (block-position db)]
        [dir (point 0 0)])
    (when (> (point-x cb-pos) (point-x db-pos))
      (set-point-x! dir HALF-B-SIZE))
    (when (< (point-x cb-pos) (point-x db-pos))
      (set-point-x! dir (- 0 HALF-B-SIZE)))
    (when (> (point-y cb-pos) (point-y db-pos))
      (set-point-y! dir HALF-B-SIZE))
    (when (< (point-y cb-pos) (point-y db-pos))
      (set-point-y! dir (- 0 HALF-B-SIZE)))
    dir))

;; INIT
(define (init)
  (define p (player (point 10 10) null))
  (define e (list (enemy (point 10 10))))
  (define l (level 0 100))
  (world p e l))

(define my-world (init))

(define (game-loop)
  (cls)
  (set! my-world (update/world my-world))
  (draw/world my-world)
  (and DEBUG (text 50 50 debug-text)))
;(draw 50 0 (hash-ref asset/sprites "player"))

(run game-loop M-WIDTH M-HEIGHT #:scale 2 #:shader #f)