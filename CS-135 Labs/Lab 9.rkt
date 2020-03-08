#lang eopl

;;*******************************************************************************
;; Name: Jonathan Joshua
;; Pledge: I pledge my honor that I have abided by the Stevens Honor System.
;;*******************************************************************************

;; In this lab, we'll implement topological sort for dags (directed acyclic graphs)!
;;
;; Topological sort is an algorithm which lists the vertices of a dag
;;   such that every edge in the dag points from an earlier vertex in the list
;;   to a later vertex in the list.
;; In other words, imagine physically moving the vertices of a dag into a line
;;   so that every edge points in the same direction. The order that the vertices
;;   are lined up in is the topological sorting of the dag.

;; We'll write topological sort in such a way that
;;   the dags don't have to be connected, and
;;   the vertices of the dag can be called anything we want,
;;   instead of just the integers 1-n like in the previous lab.

;; The topological sort algorithm goes as follows for a dag G:
;;   1. Begin with an empty output list.
;;   2. If G has no vertices, return the output list. Otherwise:
;;   3. Find the indegrees of all the vertices in G.
;;   4. Remove the lowest-indexed vertex with indegree 0 from G,
;;      and add this vertex to the end of the output list.
;;   5. Go to step 2.
;;
;; Try out this algorithm on a few dags by hand to get a feel for how it works.
;; Can you see why graphs with cycles can't be topologically sorted?


;; We'll represent a dag as a nested list of two items:
;;   a list of vertices, and
;;   a list of edges.
;; Each vertex can be represented by many different datatypes,
;;   but we'll stick to integers and strings for convenience.
;; Each edge is an ordered pair - simply a list of two vertices -
;;   where the edge points from the first vertex to the second vertex.

;; At the bottom of this file are some prewritten helper functions.
;; Read them over to see how you can take advantage of them.

;; Assumptions you may make for this program:
;;   1. Every graph used is indeed a dag. So no need to check for cycles!
;;   2. Each dag's list of vertices has no duplicates.
;;   3. Every vertex referred to in a dag's list of edges is in the dag's list of vertices.

;; Example dags:
(define dag1 '((1 2 3 4 5)
               ((2 1) (3 1) (4 1) (5 1) (4 2) (4 3) (5 3) (5 4))))
(define dag2 '(("a" "b" "c" "d" "e")
               (("a" "b") ("a" "d") ("b" "d") ("c" "b") ("c" "e") ("e" "a"))))
(define dag3 '((0 2 "sandeep" 5)
               ((5 0) (0 "sandeep") (2 "sandeep"))))
(define dag4 '((1 2 3 4)
               ((2 1) (3 4))))
(define dag5 '((1 2 3) ()))
(define dag6 '(() ()))



;; The first function to implement is indegree,
;;   which accepts a list of edges and a vertex,
;;   and returns the indegree of the vertex based on the edges.
;;
;; Recall that the indegree of a vertex is how many edges point *to* it!
;; So ensure you're checking the correct half of each edge.
;;
;; I recommend using 'equal?' for checking if vertices are the same
;;   because unlike '=', equal? works with any datatype, not just integers.

;; Some test cases:
;; (indegree (dag-edges dag1) 1) -> 4
;; (indegree (dag-edges dag1) 5) -> 0
;; (indegree (dag-edges dag3) "sandeep") -> 2
;; (indegree (dag-edges dag5) 3) -> 0

;; Type signature: (in-degree list vertex) -> integer
(define (indegree edges vertex)
  (if (null? edges)
      '0
      (if (equal? (car(cdr(car edges))) vertex)
          (+ 1 (indegree (cdr edges) vertex))
          (indegree (cdr edges) vertex))))
                         


;; Now we can write the function find-top,
;;   which accepts a dag and returns
;;   the "top" of the dag, which is
;;   the lowest-indexed vertex with an indegree of 0.
;;
;; The "index" of a vertex refers to its position
;;   in the dag's list of vertices, NOT the name of the vertex.
;; Technically, we can pick any vertex with indegree 0
;;   and the topological sort will still work. But, picking the lowest-indexed
;;   is a convention in programming, and it guarantees that everyone's
;;   implementations will produce the exact same results.
;;
;; You may assume the dag received as input has at least one vertex.
;;   We'll check for dags with no vertices in a later function which calls this one.
;; And remember, you may assume that the input graph is a valid dag,
;;   so it's guaranteed that at least one vertex will have indegree 0.

;; Some test cases:
;; (find-top dag1) -> 5
;; (find-top dag2) -> "c"
;; (find-top dag4) -> 2
;; (find-top dag5) -> 1

;; Type signature: (find-top dag) -> vertex
(define (find-top dag)
  (if (equal? 1 (length (dag-vertices dag)))
      dag-vertices dag)
  (if (equal? (indegree (dag-edges dag) (car (dag-vertices dag))) 0)
      (car (dag-vertices dag))
      (find-top (make-dag (cdr (dag-vertices dag)) (dag-edges dag)))))



;; All that's left before we can implement
;;   the final topological sort function is pop-vertex,
;;   which accepts a dag G and a vertex V,
;;   and returns a subgraph of G where V is removed from its list of vertices
;;   and all edges to/from V are removed from its list of edges.
;; Most of this function's behavior is already taken care of by helper functions
;;   at the bottom of the file. All you need to do is combine them in the right way!
;; Technically this function is capable of removing any vertex from a dag,
;;   even a vertex not in the dag (meaning nothing changes),
;;   but we'll only end up using it to remove vertices with indegree 0.

;; Some test cases:
;; (pop-vertex dag1 3) -> ((1 2 4 5) ((2 1) (4 1) (5 1) (4 2) (5 4)))
;; (pop-vertex dag2 "d") -> (("a" "b" "c" "e") (("a" "b") ("c" "b") ("c" "e") ("e" "a")))
;; (pop-vertex dag3 0) -> ((2 "sandeep" 5) ((2 "sandeep")))
;; (pop-vertex dag6 1) -> (() ())

;; Type signature: (pop-vertex dag vertex) -> dag
(define (pop-vertex dag vertex)
  (make-dag (popv-vertices (dag-vertices dag) vertex) (popv-edges (dag-edges dag) vertex)))



;; Now it's time to bring everything together and finally implement topological-sort!
;; This function accepts a dag, and returns a list
;;   of the dag's vertices in topologically sorted order.
;; Remember, the behavior of this function will be to
;;   find the "top" vertex of the dag (which we implemented already),
;;   remove said vertex from the dag (which we also implemented),
;;   and repeat this process with the modified dag until no vertices remain.

;; Test cases:
;; (topological-sort dag1) -> (5 4 3 2 1)
;; (topological-sort dag2) -> ("c" "e" "a" "b" "d")
;; (topological-sort dag3) -> (2 5 0 "sandeep")
;; (topological-sort dag4) -> (2 1 3 4)
;; (topological-sort dag5) -> (1 2 3)
;; (topological-sort dag6) -> ()

;; (topological-sort dag) -> vertex-list
(define (topological-sort dag)
  (if (null?(dag-vertices dag))
      '( )
      (cons (find-top dag) (topological-sort (pop-vertex dag(find-top dag))))))


;; Congratulations! You've implemented topological sort.
;; Now just wait a little while and you'll get to see it again in CS-385!



;;_______________________________________________________
;; Helper functions! You may utilize them, free of charge:


;; Define popv-vertices
;; Accepts a list of vertices (assumes no duplicates) and a vertex,
;; and returns the list of vertices without the given vertex.
;; Type-signature: (popv-vertices list vertex) -> list
(define (popv-vertices vertices vertex)
  (if (null? vertices) '()
      (if (equal? (car vertices) vertex)
          (cdr vertices)
          (cons (car vertices)
                (popv-vertices (cdr vertices) vertex)))))

;; Define popv-edges
;; Accepts a list of edges and a vertex,
;; and returns the list of edges with all edges to/from the given vertex removed.
;; Type-signature: (popv-edges list vertex) -> list
(define (popv-edges edges vertex)
  (if (null? edges) '()
      (if (or (equal? (caar edges) vertex)
              (equal? (cadar edges) vertex))
          (popv-edges (cdr edges) vertex)
          (cons (car edges)
                (popv-edges (cdr edges) vertex)))))

;; Define dag-vertices
;; Accepts a dag and returns the list of vertices of said dag.
;; Type signature: (dag-vertices dag) -> list
(define (dag-vertices dag) (car dag))

;; Define dag-edges
;; Accepts a dag and returns the list of edges of said dag.
;; Type signature: (dag-edges dag) -> list
(define (dag-edges dag) (cadr dag))

;; Define make-dag
;; Accepts a list of vertices and a list of edges
;;   and returns a dag with said vertices and edges.
;; Type signature: (make-dag list list) -> dag
(define (make-dag vertices edges) (list vertices edges))



;; CS-135: Discrete Structures
;; November 2019
;; Written by Jared Pincus