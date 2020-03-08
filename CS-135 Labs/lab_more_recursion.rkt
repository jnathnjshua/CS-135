#lang eopl

;;*******************************************************************************
;; Name: Jonathan Joshua
;; Pledge: I pledge my honor that I have abided by the Stevens Honor System.
;;*******************************************************************************

;; Keep in mind while testing your programs that the provided test cases may not be comprehensive.
;; Always consider any edge cases for your functions that you may need to create tests for yourself!



;; Define stopping-time
;; Returns the "stopping time" of positive integer n according to the "hailstone sequence".
;; Rules of the hailstone sequence, for some integer k > 1:
;;     If k is even, the next number in the sequence is k/2.
;;     If k is odd, the next number in the sequence is 3k+1.
;;     Repeat with each value produced, adding onto the sequence until 1 is reached.
;; The stopping time of n equals the number of steps taken in the hailstone sequence to reach 1 from n.
;;     For example, if n = 10, then the sequence begins with 10: 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1.
;;     So the stopping time for 10 is 6, because there are 6 steps to get from 10 to 1.
;; Note that it has not been proven that all values converge to 1 with this sequence (look up the Collatz conjecture if you're curious!),
;;     but it has been tested to work for all values up to hundreds of digits long.
;;     Rest assured our test cases won't be nearly that large!
;;
;; Some test cases:
;; (stopping-time 1) -> 0
;; (stopping-time 3) -> 7
;; (stopping-time 100) -> 25
;; (stopping-time 256) -> 8
;; (stopping-time 12345) -> 50

;; Type signature: (stopping-time positive-integer) -> integer
(define (stopping-time n)
  (define (stopping-time-helper n counter)
    (if (<= n 1)
        counter
        (if (even? n)
            (stopping-time-helper (/ n 2) (+ counter 1))
            (stopping-time-helper (+ (* n 3) 1) (+ counter 1)))))
  (stopping-time-helper n 0))


;; For the rest of the assignment, we will implement mergesort recursively by writing two functions.
;; First, here are three helper functions. You can take advantage of front-half and back-half.
;; Test them out to make sure you understand what they do!

;; Define front-half
;; Returns list which is the front half of the input list.
;; If length of list is odd, middle element is included.
;; Type signature: (front-half list) -> list
(define (front-half L)
  (reverse (back-half-helper (reverse L)
                             (floor (/ (length L) 2)))))

;; Define back-half
;; Returns list which is the back half of the input list.
;; If length of list is odd, middle element is excluded.
;; Type signature: (back-half list) -> list
(define (back-half L)
  (back-half-helper L (ceiling (/ (length L) 2))))

;; Helper function for back-half.
(define (back-half-helper L n)
  (if (zero? n) L
      (back-half-helper (cdr L)
                 (- n 1))))



;; Now you need to implement the first part of the mergesort algorithm - the merging!
;; The merging algorithm takes in two sorted lists and combines them into one sorted list.
;; The algorithm works as follows, given two lists A and B each already sorted from least to greatest:
;;     If both lists are empty, return an empty list.
;;     If either list is empty, return the non-empty list.
;;     Otherwise, compare the 1st element of list A to the 1st element of list B.
;;     Assign M to be the smaller 1st element between the two lists.
;;     If M was from A, remove A's 1st element. Otherwise, remove B's first element.
;;     Assign L to be the list returned by applying the merging algorithm to A and B.
;;     Return the list of M appended to the front of L.

;; Some test cases:
;; (merge '(1 2 3) '(4 5 6)) -> (1 2 3 4 5 6)
;; (merge '(2 4) '(1 3)) -> (1 2 3 4)
;; (merge '(2) '(1)) -> (1 2)
;; (merge '(1) '()) -> (1)
;; (merge '(1 4 5) '(3 5 5)) -> (1 3 4 5 5 5)

;; Type signature: (merge sorted-list sorted-list) -> sorted-list
(define (merge LA LB)
  (if (null? LA)
          LB
          (if (null? LB)
              LA
              (if (< (car LA) (car LB))
                  (cons (car LA) (merge (cdr LA) LB))
                  (cons (car LB) (merge (cdr LB) LA))))))



;; Here's where the whole mergesort algorithm will come together.
;; The mergesort function takes in a list of integers and returns the list sorted from least to greatest.
;; Mergesort algorithm with list L:
;;     Assign list A to be the result of mergesorting the front half of L.
;;     Assign list B to be the result of mergesorting the back half of L.
;;     Return the list produced by merging A and B together.
;;
;; You may notice there's no base case in the above description. What should it be?
;; Because the function is recursive, you'll need a base case in your implementation!

;; Some test cases:
;; (mergesort '(5 4 7 2 0)) -> (0 2 4 5 7)
;; (mergesort '(1 2 3)) -> (1 2 3)
;; (mergesort '(9 8 7 10 9 5 1)) -> (1 5 7 8 9 9 10)

;; Type signature: (mergesort list) -> sorted-list
(define (mergesort L)
  (if (null? L)
          L
          (if (null? (cdr L))
              L
              (merge
                (mergesort (front-half L))
                (mergesort (back-half L))))))



;; CS-135: Discrete Structures
;; October 2019
;; Written by Jared Pincus