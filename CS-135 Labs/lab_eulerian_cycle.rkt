#lang eopl

;;*******************************************************************************
;; Name: Jonathan Joshua
;; Pledge: I pledge my honor that I have abided by the Stevens Honor System.
;;*******************************************************************************


;; An undirected graph is a symmetric relation where every (a b) and (b a) pair is treated as equivalent.
;; Assume that for any undirected graph G used in this program:
;;     1. if G has N vertices then its vertices will be labeled from 1 to N.
;;     2. G is connected; in other words, a path exists from every vertex in G to every other vertex in G.

;; Here are some example undirected graphs to test your code with:
(define cycle6 '((1 2) (2 3) (3 4) (4 5) (5 6) (6 1) (2 1) (3 2) (4 3) (5 4) (6 5) (1 6)))
(define david '((3 1) (1 3) (4 2) (2 4) (5 3) (3 5) (6 4) (4 6) (1 5) (5 1) (2 6) (6 2) (1 2) (2 1) (5 4) (4 5)))
(define study '((1 6) (2 3) (3 1) (4 1) (5 1) (6 1) (2 2) (3 2) (1 3) (1 4) (1 5) (6 5) (5 6) (2 4) (4 2)))
(define test '((2 3) (4 5) (5 2) (2 1) (3 2) (1 2) (3 3) (5 4) (2 5) (1 1)))



;; Define degree:
;; degree accepts an undirected graph and a vertex,
;;     and returns the number of edges connected to said vertex.
;; To do this, check how many edges start at the same vertex as the given one.
;; Assume the input vertex is in the input relation.
;; Note: reflexive edges are special, in that they count as 2 edges but are only written once.

;; Examples:
;; (degree cycle6 4) -> 2
;; (degree david 2) -> 3
;; (degree study 1) -> 4
;; (degree test 1) -> 3

;; Type Signature: (degree relation integer) -> integer

(define (degree relation vertex)
  (if (null? relation)
      0
  (if (and (equal?(car(car relation)) vertex) (equal? (car(car relation)) (car(cdr(car relation)))))
      (+ (degree (cdr relation) vertex) 2)
  (if (equal? (car(car relation)) vertex)
      (+ (degree (cdr relation) vertex) 1)
  (degree (cdr relation) vertex)))))

;; Define max-vertex:
;; max-vertex accepts an undirected graph
;;     and returns the largest label of any of the graph's vertices.
;; Do this by recursively going through relation and storing the current max each time.
;; A helper may be useful!

;; Examples:
;; (max-vertex cycle6) -> 6
;; (max-vertex test) -> 5
;; (max-vertex '()) -> 0

;; Type Signature: (max-vertex relation) -> integer

(define (max-vertex relation)
   (max-vertex-helper relation 0))

(define (max-vertex-helper relation k)
  (if (equal? '() relation)
      k
      (if (> (caar relation) k)
          (max-vertex-helper (cdr relation) (caar relation))
          (max-vertex-helper (cdr relation) k))))



;; Define has-eulerian-cycle?
;; has-eulerian-cycle? accepts an undirected graph
;;     and returns whether the graph contains an eulerian cycle.
;; An eulerian cycle exists iff every vertex has an even degree.
;; A helper function will be needed to keep track of what vertex you are checking.
;; Note: assume that the empty relation vacuously contains an eulerian cycle.

;; Examples:
;; (has-eulerian-cycle? cycle6) -> #t
;; (has-eulerian-cycle? david) -> #f
;; (has-eulerian-cycle? study) -> #t
;; (has-eulerian-cycle? test) -> #f

;; Type Signature: (has-eulerian-cycle? relation) -> boolean

(define (has-eulerian-cycle? relation)
   (eulerian-cycle-helper relation (max-vertex relation)))

(define (eulerian-cycle-helper relation vertex)
  (if (equal? 0 vertex)
      #t
      (if (even? (degree relation vertex))
          (eulerian-cycle-helper relation (- vertex 1))
          #f)))
