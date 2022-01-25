#lang racket

(require r-cade)

(define DEBUG #f)

(define M-WIDTH 256)
(define M-HEIGHT 256)

(define B-SIZE 8)
(define HALF-B-SIZE (/ B-SIZE 2))

(define P-SPEED 1)
(define GRAVITY 2)

(define UP 0)
(define RIGHT 1)
(define DOWN 2)
(define LEFT 3)

(define PLAYER-COLOR 2)
(define MAP-COLOR 1)
(define ENEMY-COLOR 3)
(define TEXT-COLOR 4)

(define HUD-X 15)
(define HUD-Y 15)

(define LEVEL-GOLD 200)


(define debug-text "debug")

; Levels definitons
(define asset/levels (vector
                       (vector
                         (vector "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "c" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w" "l" "w" "w" "w" "w" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "c" "e" "e" "l" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "w" "w" "w" "w" "w" "l" "w" "w" "w" "w" "w" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w" "w" "l" "w" "w" "w" "w" "w")
                         (vector "w" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "l" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w")
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
                               #b01111110
                               #b10000001
                               #b10010001
                               #b10111001
                               #b10010001
                               #b10000001
                               #b01111110)
                        "en" '(
                               #b10111101
                               #b01011010
                               #b00011000
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
                              #b01111110)
                        "c" '(
                              #b00000000
                              #b00000000  
                              #b01111110
                              #b10011001
                              #b10010001
                              #b10000001
                              #b11111111)))

; Define all primitive of our game
(struct point (x y) #:transparent #:mutable)
(struct block (position coords type) #:transparent #:mutable)
(struct entity (position block) #:transparent #:mutable)
(struct player entity (gold) #:transparent #:mutable)
(struct enemy entity () #:transparent #:mutable)
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
  (define l (world-level w))
  (define e (update/enemies (world-enemies w) (level-indice l)))
  (define p (update/player (world-player w) (level-indice l)))
  (begin 
    (when ((actions/display-current-block))
      (set! debug-text (get-block (level-indice l) (entity-position p) #:mode "upper")))
    (world p e l)))

;; Entity

(define (center-entity-on-block! e)
  (let ([b (entity-block e)])
    (when (not (null? b))
      (set-entity-position! e 
                            (point 
                              (point-x (block-position b))
                              (point-y (block-position b)))))))

(define (can-entity-go? e l dir)
  (let* ([blocks (get-blocks-around-entity e l)]
         [block (list-ref blocks dir)]) 
    (and 
      (not (null? block)) 
      (not (equal? "w" (block-type block))))))

; Apply gravity to a player/enemy
(define (apply-gravity! e l)
  (let* ([position (entity-position e)]
         [x (point-x position)]
         [y (point-y position)]
         [b (entity-block e)])
    (when (and (can-entity-go? e l DOWN) (not (equal? "l" (block-type b))))
      (set-entity-position! e (point x (+ y GRAVITY))))))

(define (get-blocks-around-entity e l)
  (if (null? (entity-block e))
      (list)
      (let* ([pb (entity-block e)]
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

;; /Entity

;; Enemy
(define (update/enemies enemies l) 
  (map ((curry update/enemy) l) enemies))

(define (update/enemy l e)
  (define new-enemy (move-enemy e l))
  (begin0
    new-enemy
    (apply-gravity! new-enemy l)
    (when (point-equals? (entity-position e) (entity-position new-enemy))
      (center-entity-on-block! new-enemy))))

(define (move-enemy e l)
  (let ([x (point-x (entity-position e))]
        [y (point-y (entity-position e))])
    (enemy (point x y) (get-block l (entity-position e) #:mode "upper"))))

;; /Enemy

;; Player
(define (update/player p l)
  (define new-player (move-player p l))
  (begin0
    new-player
    (apply-gravity! new-player l)
    (collect-gold! new-player l)
    (when (point-equals? (entity-position p) (entity-position new-player))
      (center-entity-on-block! new-player))))

(define (move-player p l)
  (let ([x (point-x (entity-position p))]
        [y (point-y (entity-position p))]
        [dx 0]
        [dy 0])
    (when (and ((actions/up)) (can-entity-go? p l UP))
      (set! dy -1))
    (when (and ((actions/right)) (can-entity-go? p l RIGHT))
      (set! dx 1))
    (when (and ((actions/down)) (can-entity-go? p l DOWN))
      (set! dy 1))
    (when (and ((actions/left)) (can-entity-go? p l LEFT))
      (set! dx -1))
    (player (point
              (+ x (* P-SPEED dx))
              (+ y (* P-SPEED dy)))(get-block l (entity-position p) #:mode "upper") (player-gold p))))

(define (collect-gold! p l)
  (let ([b (entity-block p)])
    (when (equal? "c" (block-type b))
      (set-player-gold! p (+ 100 (player-gold p)))
      (persist-block! 
        (block (block-position b) (block-coords b) "e")
        l))))

;; /Player

;; DRAW
; Little helper to draw color palette 
(define (debug/draw-palette)
  (for ([i (range 16)])
    (color i)
    (draw 0 i '(#xff))))

(define (draw/world w)
  (let* ([indice (level-indice (world-level w))]
         [player (world-player w)]
         [enemies (world-enemies w)])
    (draw/level indice)
    (draw/player player indice)
    (draw/enemies enemies indice)
    ;; HUD
    (draw/player-stats player)))
  
; Draw a specific level (l correspond to level indice in asset/levels)
(define (draw/level i)
  (color MAP-COLOR)
  (define (draw-block b)
    (let ([x (point-x (block-position b))]
          [y (point-y (block-position b))]
          [t (block-type b)])
      (when (hash-has-key? asset/sprites t)
        (draw x y (hash-ref asset/sprites t)))))
  (on-level-map i draw-block))

; Draw player
(define (draw/player-stats p)
  (let ([gold (number->string (player-gold p))]
        [level-gold (number->string LEVEL-GOLD)])
    (color TEXT-COLOR)
    (text HUD-X HUD-Y (string-append "Gold: " gold "/" level-gold))))

(define (draw/player p l)
  (color PLAYER-COLOR)
  (let* ([x (point-x (entity-position p))]
         [y (point-y (entity-position p))]
         [debug-blocks (cons (entity-block p) (get-blocks-around-entity p l))])
    (draw x y (hash-ref asset/sprites "p"))
    (and DEBUG (map (lambda (b)
                      (unless (null? b)
                        (let ([bx (point-x (block-position b))]
                              [by (point-y (block-position b))])
                          (draw bx by (hash-ref asset/sprites "db"))))
                      ) debug-blocks))))

(define (draw/enemies enemies l)
  (color ENEMY-COLOR)
  (map ((curry draw/enemy) l) enemies))

; Draw Enemy
(define (draw/enemy l e)
  (let ([x (point-x (entity-position e))]
        [y (point-y (entity-position e))])
    (draw x y (hash-ref asset/sprites "en"))))

;; HELPERS
(define (on-level-map i f)
  (let ([x 0] [y 0] [level (vector-ref asset/levels i)])
    (for ([r level])
      (set! x 0)
      (for ([t r])
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

; Persit (update map) a block to a level l 
(define (persist-block! b l)
  (when (block-in-map? b l)
    (let ([bc (point-x (block-coords b))]
          [bl (point-y (block-coords b))]
          [bt (block-type b)]
          [level (vector-ref asset/levels l)])
      (vector-set! (vector-ref level bl) bc bt))))

; Get direction from cb block to db block
(define (get-block-direction cb db)
  (let* ([cb-pos (block-position cb)]
         [db-pos (block-position db)]
         [dir (point 
                (- (point-x cb-pos) (point-x db-pos))
                (- (point-y cb-pos) (point-y db-pos)))])
    dir))

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
  (define p (player (point 50 50) null 0))
  (define e (list (enemy (point 10 10) null)))
  (define l (level 0 300))
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