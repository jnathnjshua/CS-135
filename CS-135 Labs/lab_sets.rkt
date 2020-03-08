#lang eopl

;;*******************************************************************************
;; Name: Jonathan Joshua
;; Pledge: I pledge my honor that I have abided by the Stevens Honor System.
;;*******************************************************************************

;; element?, intersection, set-difference are the only functions that cannot be done in a simple one line function. 

;; As you complete functions, see if you can utilize them when writing subsequent functions!

;;list: a grouping of elements (duplicate elements allowed)
;;set: a grouping of unique elements (no duplicates)
;;Be careful. Some of these functions take lists and return sets.

;; Define element?
;; Given item, and list-of-items, return #t if item is in list-of-items, #f otherwise

;; Examples:
;; (element? 0 '()) => #f
;; (element? 8 '(7 8 9)) => #t
;; (element? 7 '(1 2 3 4)) => #f
;; (element? 'saw '(the man saw a dog)) => #t


;; Type signature: (element? element list) -> boolean

(define (element? item list-of-items)
  (if (null? list-of-items) #f
      (if (equal? item (car list-of-items)) #t
          (element? item (cdr list-of-items)))))
      


;; make-set takes a list and removes all duplicates
;; This will be useful for the rest of our functions

;; Type signature: (make-set list) -> set

(define (make-set list-of-items)
  (cond
    [(null? list-of-items)
     '()] ; Empty lists never have duplicates
    [(element? (car list-of-items) (cdr list-of-items))
     (make-set (cdr list-of-items))]
    [else
     (cons (car list-of-items) (make-set (cdr list-of-items)))]))


;; Define union
;; Given two lists, return a SET containing all of the elements from either list.

;; NOTE: Order does not matter when checking your work for union

;; Examples:
;; (union '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6) 
;; (union '(1 2 3) '(1 2 3)) => '(1 2 3)
;; (union '(1 1 1) '()) => '(1)

;; Type signature: (union list list) -> set

(define (union listA listB)
  (make-set(cond ((null? listB) listA)
        ((member (car listB) listA)
         (union listA (cdr listB)))
        (else (union (cons (car listB) listA) (cdr listB))))))


;; Define intersection
;; Given two lists A and B, return the set which
;; contains all elements in both A and B.

;; NOTE: Order will not matter when checking your work for intersection

;; Examples:
;; (intersection '(1 2 3 4) '(2 4 5)) => '(2 4)
;; (intersection '(s a n d e e p) '(b h a t t)) => '(a)
;; (intersection '(c c c) '(c c c)) => '(c)
;; (intersection '(a a a) '()) => '()

;; Type signature: (intersection list list) -> set

(define (intersection listA listB)
  (make-set(if (null? listA)
      '()
      (if (member (car listA) listB)
          (cons (car listA) (intersection (cdr listA) listB))
          (intersection (cdr listA) listB)))))


;; Define subset?
;; Takes two sets and returns whether the first is a subset of the second.
;; (i.e., every element in the first is also in the second set)

;; Examples:
;; (subset? '() '()) => #t
;; (subset? '(1 2 3) '(1 2 3 4 5)) => #t
;; (subset? '(115 284 385) '(115 146 284 135 385 392)) => #t
;; (subset? '(-2 0 2) '(-1 1 3)) => #f
;; (subset? '(-1 1 2) '(-1 1 3 5 7)) => #f

;; Type signature: (subset? set set) -> boolean

(define (subset? setA setB)
    (or (null? setA)
        (and (member (car setA) setB)
             (subset? (cdr setA) setB))))


;; Define set-equal?
;; Determines whether two sets are equivalent (i.e. A = B -> every element
;; in A is in B and every element in B is in A)
;; NOTE: order does not matter, so you cannot simply use (equal? A B).
;; NOTE: For the simplest solution, this can be a one-liner

;; Examples:
;; (set-equal? '() '()) => #t
;; (set-equal? '(a b c) '(a b c)) => #t
;; (set-equal? '(1 2 3 4) '(4 3 1 2)) => #t
;; (set-equal? '(1 2 3) '(1 2 4)) => #f

;; Type signature: (set-equal? set set) -> boolean

(define (set-equal? setA setB)
  (and (subset? setA setB)
         (subset? setB setA)))


;; Define set-difference
;; The set difference A - B is the set of all elements of A
;; which are not in list B.

;; Examples:
;; (set-difference '(1 2 3) '(2 3 4)) => '(1)
;; (set-difference '(1 2 3) '(1 2 3)) => '()
;; (set-difference '(1 2 3) '(4 5 6)) => '(1 2 3)
;; (set-difference '() '(1 2 3))      => '()
;; (set-difference '(1 1 2 3 3) '())  => '(1 2 3)

;; Type signature: (set-difference list list) -> set

(define (set-difference listA listB)
  (make-set(cond ((null? listA)
         '())
        ((not (member (car listA) listB))
         (cons (car listA) (set-difference (cdr listA) listB)))
        (else
         (set-difference (cdr listA) listB)))))


;; Define sym-diff
;; The symmetric difference of two lists A and B is the union of A - B and B - A.
;; (It is the set equivalent of the logical operator XOR.)

;; Examples:
;; (sym-diff '(1 2 3) '(3 4 5)) => '(1 2 4 5)
;; (sym-diff '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6)
;; (sym-diff '(1 2 3) '(1 2 3)) => '()
;; (sym-diff '(1 2) '(1 2 3 4)) => '(3 4)
;; (sym-diff '(1 1 1) '()) => '(1)

;; Type signature: (sym-diff list list) -> set

(define (sym-diff listA listB)
  (union (set-difference listA listB)(set-difference listB listA)))



;; Define cardinality
;; the cardinality of a list |A| is the number of unique elements in A,
;; or the length of A as a set.

;; Examples:
;; (cardinality '(1 2 3))    => 3
;; (cardinality '(1 1 2 3 3) => 3
;; (cardinality '(1 1 1 1 1) => 1
;; (cardinality '() )        => 0

;; Type signature: (cardinality list) -> int

(define (cardinality lst . dl)
  (let ((dl (if (null? dl) '() (car dl))))
    (cond ((null? lst) 0)
          ((member (car lst) dl)
           (cardinality (cdr lst) dl))
          (else
           (+ 1 (cardinality (cdr lst) (cons (car lst) dl)))))))


;; Define disjoint
;; Two sets are disjoint if they don't have any elements in common.
;; I.e., their intersection is empty.

;; Examples:
;; (disjoint? '(1 2 3) '()) => #t
;; (disjoint? '(1 2 3) '(1) => #f
;; (disjoint? '(1 2 3) '(4 5 6) => #t

;; Type signature: (disjoint? set set) -> boolean

(define (disjoint? setA setB)
  (null? (intersection setA setB)))


;; Define superset?
;; A is a superset B if every element in B is an element of A.
;; i.e. (A >= B) <==> (B <= A)

;; Examples:
;; (superset? '() '()) => #t
;; (superset? '(1 2 3 4 5) '(1 2 3)) => #t
;; (superset? '(-1 1 3) (-2 0 2)) => #f

;; Type signature: (superset? set set) -> boolean

(define (superset? setA setB)
  (subset? setA setB)))


;; Define insert
;; Takes a list and an element and returns the set with the element
;; added to the set.
;; Remember, if the new element was already in the set,
;; the set does not change.

;; Examples:
;; (insert 0 '(1 2 3)) => '(0 1 2 3)  [or '(1 2 3 0) etc.]
;; (insert 1 '(1 2 3)) => '(1 2 3)
;; (insert 0 '(0 0 0)) => '(0)

;; Type signature: (insert element list) => set

(define (insert element lst)
  (make-set(cond ((null? lst)
                  (list element))
                 ((<= element (car lst))
                  (cons element lst))
                 (else
                  (cons (car lst)
                        (insert element (cdr lst))))))) 


;; Define remove
;; Takes an element and a set and returns the set without that element.
;; If the element to be removed is not in the set,
;; return the set unchanged rather than raising an exception.

;; (remove 2 '(1 2 3)) => '(1 3)
;; (remove 3 '(3))     => '()
;; (remove 4 '(1 2 3)) => '(1 2 3)

;; Type signature: (remove element set) => set

(define (remove element set)
  (if (null? set)
      '()
      (cond
    ((equal? element (car set)) (cdr set))
    (else (cons (car set) (remove element (cdr set)))))))


;; January 2018
;; Samuel Kraus and Edward Minnix
;; Stevens Institute of Technology
;; CS 135  Discrete Structures

;; Updated September 2019 by Jared Pincus