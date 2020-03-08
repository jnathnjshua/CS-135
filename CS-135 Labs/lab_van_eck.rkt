#lang eopl

;;*******************************************************************************
;; Name:
;; Pledge:
;;*******************************************************************************

;; Define van-eck
;; Returns the nth number in van Eck's sequence, as defined here:
;;    https://en.wikipedia.org/wiki/Van_Eck%27s_sequence
;; Assume that the sequence begins at index 0.
;; Recommendations:
;;    Creating helper function(s) could be helpful.
;;    Look closely at how eopl's 'member' function works!

;; Some test cases:
;; (van-eck 0) -> 0
;; (van-eck 4) -> 2
;; (van-eck 95) -> 8
;; (van-eck 1000) -> 61

;; Type signature: (van-eck integer) -> integer
(define (van-eck n)
  "Don't bother the other CAs with this one. Just ask Jared.")



;; CS-135: Discrete Structures
;; October 2019
;; Written by Jared Pincus