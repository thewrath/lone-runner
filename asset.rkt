#lang racket

(provide asset/sprites)
(provide asset/levels)

; Sprites defintions
(define asset/sprites (hash
                        "p" '(
                              #b00111100
                              #b00011000
                              #b00011000
                              #b01111110
                              #b01000010
                              #b01000010
                              #b01000010)
                        "d" '(
                              #b11111111
                              #b10000001
                              #b10000001
                              #b10000001
                              #b10000101
                              #b10000001
                              #b10000001)
                        "l" '(
                              #b01000010
                              #b01000010
                              #b01000010
                              #b01111110
                              #b01000010
                              #b01000010
                              #b01000010)
                        "r" '(
                              #b11111111
                              #b00000000
                              #b00000000
                              #b00000000
                              #b00000000
                              #b00000000
                              #b00000000)
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
                        "h" '(
                              #b00000000
                              #b00000000  
                              #b10000001
                              #b11000011
                              #b10111101
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

; Levels definitons 
(define asset/levels (vector
                       (vector
                         (vector "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w" "l" "w" "w" "w" "w" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "l" "w" "w" "w" "w" "w" "w" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "l" "e" "c" "e" "e" "w")
                         (vector "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "l" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "l" "w" "w" "w" "w" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "c" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w" "l" "w" "w" "w" "w" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "r" "r" "r" "r" "r" "r" "r" "r" "r" "r" "r" "r" "r" "r" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "c" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "c" "e" "e" "l" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "w" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "w" "w" "w" "w" "w" "l" "w" "w" "w" "w" "w" "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w" "w" "l" "w" "w" "w" "w" "w")
                         (vector "w" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "w")
                         (vector "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "l" "w" "w" "w" "w" "w" "e" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "l" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "e" "w")
                         (vector "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w" "w")
                         )))