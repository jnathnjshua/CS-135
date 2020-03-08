#lang eopl

;; "I pledge my Honor that I have abided by the Stevens Honor System." - Jonathan Joshua



;; This lab is an introduction to using lists in Scheme

;; All lists in Scheme are linked lists, meaning lists
;; are either null or a pair of a head and tail.

;; Scheme has the tick operator ' also called "quote"
;; The tick operator is used to tell the interpreter to treat a symbol
;; as a symbol rather than evaluating the expression.
;; When you quote a list, it quotes each sub-expression in the list.

;; Building Lists

'()  ;; This is how you write an empty list

;; To build a list of multiple values, use the "list" function

;; (list 1 2 3)
;; (list 'a 'b)

;; Define a function "name" that given a first and last name, returns a list
;; with the first and last names.
;; Example: (name "Sandeep" "Bhatt") -> ("Sandeep" "Bhatt")

;; Type signature: (name string string) -> string-list
(define (name first last)
  (list first last))


;; To check if a list is empty, use the "null?" function.
;; Type signature: (null? list) -> boolean

;; To check how many elements are in a list, use "length"
;; Type signature: (length list) -> integer 

;; To reverse the order of elements in a list, use "reverse"
;; Type signature: (reverse list) -> list

;; To add one element to the beginning of a list, we use the "cons" function
;; Type signature: (cons element list) -> list

;; To combine two lists, use the "append" function
;; Type signature: (append list list) -> list

;; To get the first element of a list, use the function "car"
;; Type signature: (car list) -> element

;; To get everything but the first element in the list, use the "cdr" function
;; Type signature (cdr list) -> list

;; NOTE: car and cdr throw exceptions when handed empty lists

;; NOTE: '(a b c d) is the same as (list 'a 'b 'c 'd)

;; Define a function lastName that takes a list containing
;; '(first-name last-name) and returns last-name

(define (lastName name)
  (car (cdr name)))


;; In Scheme you can have lists of lists, a.k.a. "nested lists"
;; A "student" is represented with a nested list of the following structure:

(define example-student
  '((IDnumber Degree)
    (LastName FirstName)
    (day month year)
    (class-year ((major) (minor)) GPA)
    ((number street apt) (city state zip))
    (class1 class2 ... classN)))


;; Here are two example students to test your functions with:

(define stu1
  '((12345 "Bachelor of Science")
    ("Dongle" "Jonathy")
    (29 "February" 1999)
    (2021 (("Computer Science") ("Math")) 3.75)
    ((5 "Bubble Street" 16) ("Hoboken" "NJ" "07030"))
    ("CS-334" "CS-385" "MA-331" "BT-353")))

(define stu2
  '((10101010101 "Bachelor of Science")
    ("Sprimpling" "Sir Ardlinton")
    (1 "December" 1852)
    (1874 (("Engineering") ("Literature")) 4.000001)
    ((999 "Road Street" 11) ("Old Town Place" "MA" "00001"))
    ("MA-121" "CAL-103" "PE-200" "CH-115" "E-101")))

;; Based on the student template, complete the following definitions
;; using car and cdr

;; Example:
(define (birthday student)
  (car (cdr (cdr student))))
;; Since the birthday is the third element in the student list, we can access it
;; by dropping the first two elements (by using cdr twice) and using car to get the
;; first remaining element.
;; To test these functions you can call, for example, (birthday stu1) and that should return (29 "February" 1999)

;; NOTE: Racket has extra functions for shorthands of nesting car and cdr,
;;    so birthday's function body could also be written as (caddr student)
;; A shorthand function exists for every permutation of up to 4 car's and cdr's. Here they all are:
;;    https://docs.racket-lang.org/reference/pairs.html#%28part._.Pair_.Accessor_.Shorthands%29

;; Now define:

;; Returns the 'IDnumber field
(define (IDnumber student)
  (car (car student)))

;; Returns '((number street apt) (city state zip))
(define (address student)
  (car (cdr (cdr (cdr (cdr student))))))

;; Returns the 'GPA field
(define (GPA student)
  (car (reverse (car (cdr (cdr (cdr student)))))))

;; Returns the 'state field
(define (state student)
  (car (cdr (car (reverse (car (cdr (cdr (cdr (cdr student))))))))))

;; Define a function pig-latin using car, cdr, cons, and append
;; It follows the rules of Pig Latin, so
;; (pig-latin '(h a p p y)) => '(a p p y h a y)
;; (pig-latin '(b i r t h d a y)) => '(i r t h d a y b a y)
;; Type signature (pig-latin symbol-list) -> symbol-list

;; You can use this variable "ay" instead of '(a y) in your definition

(define ay '(a y))

(define (pig-latin word)
  (append (append (cdr word) (list (car word)) ay)))


;; Define a function (yoda 3-words) that takes a list of three words
;; and returns it in Yoda-speak (i.e. (w1 w2 w3) becomes (w3 w1 w2))
;; using the functions you learned above.

;; (yoda '(I am Groot)) => '(Groot I am)
;; (yoda '(I love racket)) => '(racket I love)
;; (yoda '(Sandeep is shiny-headed)) => '(shiny-headed Sandeep is)

(define (yoda 3-words)
  (append (cdr (cdr 3-words)) (reverse (cdr (reverse 3-words)))))


;; January 2018
;; Samuel Kraus and Edward Minnix
;; Stevens Institute of Technology
;; CS 135  Discrete Structures

;; Updated September 2019 by Jared Pincus
