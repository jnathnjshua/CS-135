#lang eopl

;; Write your name and pledge here!


;; Lab #3
;; Introduction to Recursion with Lists in Scheme
;; Unless explicitly stated, the empty list is valid input on every function

;; nth
;; This gets the N-th element of the list, where 0 represents the
;; first element of the list.
;; You don't need to worry about negatives or indices greater than the length
;; of the list.

;; Examples:
;; (nth 1 '(Sandeep Owen Jared James Alex Kurt)) => 'Owen
;; (nth 5 '("zero" "one" "two" "three" "four" "five")) => "five"
;; (nth 0 '(a b c)) => 'a

;; Type signature: (nth integer list) -> element from list

(define (nth n list)
  (if (equal? n 0)
       (car list)
  (nth (- n 1) (cdr list))))


;; map
;; This function applies a function to every element in a list
;; Map takes a function f and a list lst. It recursively goes
;; through the list, applying f to each element in the list.

;; Examples:
;;
;; (define (double x) (* x 2))
;; (map double '(1 2 3 4)) => '(2 4 6 8)
;; (map zero? '(0 0 1 2)) => '(#t #t #f #f)

;; Type signature: (map unary-function list) -> list

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map f (cdr lst)))))

;; filter
;; Filter takes a predicate function (one that returns a boolean)
;; and a list. It recursively goes through the list, removing any
;; element for which (pred element) returns false and keeps any element
;; where (pred element) returns #t.

;; Examples
;; (filter zero? '(1 0 2 34 56 1 0)) => '(0 0)
;; (filter even? '(0 1 2 3 4 5 6 7 8 9)) => '(0 2 4 6 8)
;; (filter number? '(shave and 1 haircut 2 bits)) => '(1 2)

;; Type signature: (filter unary-predicate list) -> list
;; where a unary predicate takes one element and returns a boolean

(define (filter pred lst)
  (define (answer lst result)
    (cond
      ((null? lst) (reverse result))
      ((pred (car lst)) (answer (cdr lst)
                           (cons (car lst) result)))
      (else (answer (cdr lst)
                  result))))
  (answer lst '()))


;; sum
;; Returns the summation of every element in the list.
;; You can assume that the list sum receives will not contain non-numbers.

;; (sum (list 1 3 5 1)) => 10

;; Type signature: (sum number-list) -> number

(define (sum lst)
  (if
    (null? lst)
    0
    (+ (car lst) (sum (cdr lst)))))


;; product
;; Returns the product (result of multiplying) of every element in the list.
;; You can assume that the list product receives will not contain non-numbers.

;; (product (list 1 3 5 4)) => 60

;; Type signature: (product number-list) -> number

(define (product lst)
  (let loop ((lst lst) (accum 1))
    (cond
      ((null? lst) accum)
      ((not (number? (car lst))) '())
      (else (loop (cdr lst) (* accum (car lst)))))))


;; January 2018
;; Samuel Kraus and Edward Minnix
;; Stevens Institute of Technology
;; CS 135  Discrete Structures

;; Updated September 2019 by Jared Pincus
