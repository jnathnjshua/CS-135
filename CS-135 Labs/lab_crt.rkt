#lang eopl

;;*******************************************************************************
;;   Name: Jonathan Joshua
;; Pledge: "I pledge my honor that I have abided by the Stevens Honor System."
;;*******************************************************************************

;; In this lab, you'll code the Chinese Remainder Theorem algorithm with several small functions.
;;
;; Throughout the lab we'll represent systems of linear congruences with lists of integer pairs,
;;   where each pair (a b) represents the congruence x ≡ a (mod b).
;; For example, the "cong-sys" '((10 11) (4 12) (12 13)) represents the system:
;;   x ≡ 10 (mod 11)
;;   x ≡  4 (mod 12)
;;   x ≡ 12 (mod 13)
;;
;; You may assume throughout the lab that any system passed through to functions is not empty.


;; Helper functions for Euclid's Algorithm and the Pulverizer are provided below:

;; Define euclid-gcd
;; Given non-negative integers a and b, returns gcd(a,b).
;;
;; Type Signature: (euclid-gcd int int) -> int
(define (euclid-gcd a b)
  (if (zero? b) a
      (euclid-gcd b (modulo a b))))

;; Define pulverize
;; Given non-negative integers a and b,
;;   returns the list (gcd(a,b) s t) where gcd(a,b) = s*a + t*b.
;;
;; Note: if you analyze this function,
;;   you'll see it uses a function called 'let'.
;; 'let' basically allows you to declare constants
;;   so you can compute something once and use the result multiple times
;;   instead of recomputing the result for every time you need it.
;; You may NOT use 'let' or other related functions on these lab assignments,
;;   but you MAY use them for the tree method extra credit!
;;
;; Type Signature: (pulverize int int) -> list
(define (pulverize a b)
  (if (zero? a)
      (list b 0 1)
      (let ([p (pulverize (modulo b a) a)])
        (list (car p)
              (- (caddr p)
                 (* (quotient b a) (cadr p)))
              (cadr p)))))



;; Define mul-inv
;; Given non-negative integers a and b,
;;   returns integer x such that a*x ≡ 1 (mod b).
;; In other words, it returns the modular multiplicative inverse of a (mod b).
;; You may assume that a and b are relatively prime.
;;
;; Hint: how do we compute modular multiplicative inverses by hand?
;;   (Take advantage of the 'pulverizer' helper function!)
;;
;; Some test cases:
;; (mul-inv 31 76) -> 27
;; (mul-inv 127 555) -> 118
;; (mul-inv 1234 4321) -> -1082

;; Type Signature: (mul-inv int int) -> int
(define (mul-inv a b)
  (car (cdr (pulverize a b))))


;; Define m
;; Given a system of linear congruences,
;;   returns the value of "m" in the CRT process,
;;   which is the product of all moduli in the system.
;; This should be a simple multiplication recursion on the second number in each pair.
;;   (Or an even simpler one-liner if you figure out EOPL's 'apply' and 'map' functions!
;;      ~ Jared's sneaky Scheme hint of the day)

;; Some test cases:
;; (m '((2 3) (3 5) (2 7))) -> 105
;; (m '((10 11) (4 12) (12 13))) -> 1716
;; (m '((1 5) (2 14) (5 23) (26 27))) -> 43470

;; Type Signature: (m cong-sys) -> int
(define (m cong-sys)
  (if (null? cong-sys)
      1
      (* (car (cdr (car cong-sys))) (m (cdr cong-sys)))))



;; Define CRT-exists?
;; Given a system of linear congruences,
;;   returns a boolean stating if CRT is possible.
;; CRT is possible iff all moduli in the system are pairwise relatively prime.
;;
;; To determine this, you must check each possible pair of the system's moduli.
;; Compare the first modulo with every other modulo,
;;   and return false if the GCD of any pair isn't 1.
;; If all the GCDs are 1, recurse on the list without the first element
;;   and check again until the list is empty, in which case return true.
;; You'll probably want to write a helper function!

;; Some test cases:
;; (CRT-exists? '((2 3) (3 5) (2 6))) -> #f
;; (CRT-exists? '((10 11) (4 12) (12 13))) -> #t
;; (CRT-exists? '((1 5) (2 14) (5 23) (26 28))) -> #f
;; (CRT-exists? '((1 2) (1 3) (1 5) (1 7) (1 11) (1 13))) -> #t

;; Type Signature: (CRT-exists? cong-sys) -> boolean
(define (CRT-exists? cong-sys)
  (CRT-help? cong-sys (car (cdr (car cong-sys))) (cdr cong-sys)))

(define (CRT-help? cong-sys l list)
  (if (equal? (cdr cong-sys) '())
      #t
      (if (equal? list '())
          (CRT-help? (cdr cong-sys) (car (cdr (car (cdr cong-sys)))) (cdr (cdr cong-sys)))
          (if (equal? (euclid-gcd l (car (cdr (car list)))) 1)
              (CRT-help? cong-sys l (cdr list))
              #f))))

;; Define CRT-helper
;; Given a valid cong-sys (one where CRT exists) and m (the product of all the moduli),
;;   returns the solution to the system via CRT without simplifying.
;;
;; To do this, you need to return the summation Ai*Mi*yi for all i in the system:
;;   Ai is the first element of each integer pair in the cong-sys list.
;;   Mi is m/b where b is the second element of each pair.
;;   yi is the multiplicative inverse of Mi (mod Bi).

;; Some test cases:
;; (CRT-helper '((10 11) (4 12) (12 13)) 1716) -> -17876
;; (CRT-helper '((2 3) (3 5) (2 7)) 105) -> 23
;; (CRT-helper '((1 2) (2 3) (3 5) (4 7) (5 11) (6 13)) 30030) -> -30817

;; Type Signature: (CRT-helper cong-sys int) -> int
(define (CRT-helper cong-sys m)
  (if (null? cong-sys)
      0
  (+ (* (car(car cong-sys)) (/ m (car(cdr(car cong-sys)))) (mul-inv (/ m (car(cdr(car cong-sys)))) (car(cdr(car cong-sys))))) (CRT-helper (cdr cong-sys) m))))



;; Define CRT
;; This brings together everything you've written so far to calculate
;;   the solution to a system of linear congruences with the CRT process.
;;
;; First, check if CRT is possible to be computed with the given cong-sys.
;; If CRT isn't possible, return -1.
;; If CRT is possible, find the unsimplified solution X to the system with CRT-helper,
;;   then return the simplified solution, which is the smallest positive integer
;;   congruent to X (mod m).

;; Some test cases:
;; (CRT '((10 11) (4 12) (12 13))) -> 1000
;; (CRT '((2 3) (3 5) (2 7))) -> 23
;; (CRT '((1 2) (2 3) (3 5) (4 7) (5 11) (6 13))) -> 29243
;; (CRT '((1 2) (4 8) (8 9))) -> -1

;; Type Signature: (CRT cong-sys) -> int
(define (CRT cong-sys)
  (if (CRT-exists? cong-sys) (modulo (CRT-helper cong-sys (m cong-sys)) (m cong-sys)) -1))
