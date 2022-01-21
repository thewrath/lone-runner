#lang racket

(require r-cade)

(define M-WIDTH 256)
(define M-HEIGHT 256)

(define B-SIZE 8)

(define P-SPEED 1)

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
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "l" "w" "w" "w" "w" "w" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         #("w" "e" "e" "e" "e" "e" "s" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
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
(struct point (x y) #:transparent)
(struct block (position type) #:transparent)
(struct player (position) #:transparent)
(struct enemy (position) #:transparent)
(struct level (indice gold) #:transparent)
(struct world (player enemies level) #:transparent)

(define (point-equals? a b) 
  (and 
    (= (point-x a) (point-x b)) 
   	(= (point-y a) (point-y b))))

;; UPDATE
; Player actions
(define (actions/up) (action btn-up #t))
(define (actions/right) (action btn-right #t))
(define (actions/down) (action btn-down #t))
(define (actions/left) (action btn-left #t))

(define (update/world w) 
  (define e (world-enemies w))
  (define l (world-level w))
  (define p (update/player (world-player w) (level-indice l)))
  (world p e l))

(define (update/player p l)
  (define pos (player-position p))
  (define new-player (move-player (player (apply-gravity pos))))
  (if (not (on-wall? (player-position new-player)))
      new-player
      p))

(define (move-player p)
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
            (+ y (* P-SPEED dy)))))

; Center position on a nerest block
; l for next position
; np for next position
; lp for last position
(define (center-on-level-block l np lp)
  (define block-round-mode (if (< (point-x np) (point-x lp)) "lower" "upper"))
  (define block (get-block l np #:mode block-round-mode)) 
  (if (null? block)
      np
      np))

; Check that the position does not exceed the walls
(define (on-wall? p)
  (let ([x (point-x p)]
        [y (point-y p)])
  (or (< x B-SIZE) ; left
      (< y B-SIZE) ; up
      (> x (- M-WIDTH (* 2 B-SIZE))) ; right
      (> y (- M-HEIGHT (* 2 B-SIZE)))))) ; down

; Apply gravity to a position
(define (apply-gravity p)
  (if (< (point-y p) (- M-HEIGHT (* 2 B-SIZE)))
      (point 
        (point-x p)
        (add1 (point-y p)) )
      p))

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

;; INIT
(define (init)
  (define p (player (point 10 10)))
  (define e (list (enemy (point 10 10))))
  (define l (level 0 100))
  (world p e l))

(define my-world (init))

(define (game-loop)
  (cls)
  (set! my-world (update/world my-world))
  (draw/world my-world))
;(draw 50 0 (hash-ref asset/sprites "player"))

(run game-loop M-WIDTH M-HEIGHT #:scale 2 #:shader #f)