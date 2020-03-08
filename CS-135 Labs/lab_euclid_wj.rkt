#lang eopl

;;*******************************************************************************
;;   Name: Jonathan Joshua
;; Pledge: "I pledge my honor that I have abided by the Stevens Honor System."
;;*******************************************************************************

;; EOPL functions to consider:
;;   (modulo a b) returns a mod b (a % b in Python).
;;   (quotient a b) returns the integer quotient of a ÷ b (a // b in Python).
;;   You may NOT use EOPL's built-in 'gcd' function!



;; Define euclid-gcd
;; This function uses Euclid's Algorithm to find the GCD of two numbers.
;; It works by using the fact that gcd(a, b) = gcd(b, a mod b).
;; Once 'b' is 0, 'a' will be the GCD of the two numbers.
;; Make sure that your function returns the correct values for inputs like 0 and 1!

;; Some test cases:
;; (euclid-gcd 37 1234) -> 1
;; (euclid-gcd 1532732 180) -> 4
;; (euclid-gcd 1234 43210) -> 2

;; Type signature: (euclid-gcd int int) -> int
(define (euclid-gcd int1 int2)
  (if (zero? int2)
      int1
      (euclid-gcd int2 (modulo int1 int2))))



;; Define euclid-list
;; This is very similar to euclid-gcd
;;   but it keeps track of all values of int1.
;; So the output should be a list of
;;   what int1 is at all points in the algorithm.
;; The first number should be what 'a' is initially
;;   and the last number should be the GCD.

;; Some test cases:
;; (euclid-list 1234 24) -> '(1234 24 10 4 2)
;; (euclid-list 17 4) -> '(17 4 1)
;; (euclid-list 51 100) -> '(51 100 51 49 2 1)

;; Type signature: (euclid-list int int) -> int-list
(define (euclid-list int1 int2)
  (if (zero? int2)
      (list int1)(cons int1 (euclid-list int2 (modulo int1 int2)))))



;; Define wj-help
;; This helper function, in conjunction with 'waterjug' below,
;;   solves the water jug problem with 2 jugs.
;; It returns the list of fill levels that
;;   both jugs have at each step of the solution.
;; So, the returned list should contain the pair (fillA fillB)
;;   for each recursive step.
;;
;; As a reminder, the objective of the water jug problem,
;;   given two jugs with capacities A and B and a pool of water,
;;   is to measure out exactly C units of water.
;; The only operations you can perform are completely filling a jug,
;;   completely emptying a jug, or pouring water from one jug into the other.
;;
;; There are 5 cases to consider for each step of the algorithm:
;;   1. fillB = goal  -> done!
;;   2. fillB = maxB  -> empty jug B
;;   3. fillA = 0  -> fill jug A
;;   4. fillA + fillB > maxB  -> pour some of jug A into jug B to fill jug B
;;   5. fillA + fillB <= maxB  -> pour all of jug A into jug B
;;
;; You may assume that for the values passed into the 'waterjug' function:
;;   1. jugA < jugB
;;   2. A solution does exist for jug capacities jugA and jugB and target fill 'goal'.
;; Notes:
;;   1. To make a pair and insert it into a list, use:
;;     (cons (list item1 item2) rest-of-list)
;;   2. Make sure the final pair you insert into the list are a pair rather than two individual elements.
;;   3. Use a 'cond' statement! Or just nest a bunch of 'if' statements if you really want to...

;; Some test cases:
;; (waterjug 3 5 4) -> ((0 0) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4))
;; (waterjug 10 12 6) -> ((0 0) (10 0) (0 10) (10 10) (8 12) (8 0) (0 8) (10 8) (6 12) (6 0) (0 6))
;; (waterjug 13 17 9) -> ((0 0) (13 0) (0 13) (13 13) (9 17) (9 0) (0 9))

;; Type signature: (wj-help int int int int int) -> list
(define (wj-help fillA fillB maxA maxB goal)
  (cond
    ((equal? fillB goal)(list (list fillA fillB)))
    ((equal? fillB maxB)(cons (list fillA fillB) (wj-help fillA 0 maxA maxB goal)))
    ((equal? fillA 0)(cons (list fillA fillB) (wj-help maxA fillB maxA maxB goal)))
    ((> (+ fillA fillB) maxB)(cons(list fillA fillB)(wj-help(- fillA(- maxB fillB)) maxB maxA maxB goal)))
    ((<= (+ fillA fillB) maxB) (cons (list fillA fillB)(wj-help 0 (+ fillA fillB) maxA maxB goal)))))

;; define waterjug
;; waterjug accepts two jug capacities and a goal volume
;;   and returns a list of the steps of the solution
;;   to the water jug problem with those values.

;; Type signature: (waterjug int int int) -> list
(define (waterjug jugA jugB goal)
  (wj-help 0 0 jugA jugB goal))
