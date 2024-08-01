#lang racket

(require redex)

(define-language cas
  (c
   number)
  (x variable-not-otherwise-mentioned)
  (e
   c
   x
   (~ e) ; negation
   (+ e e) ; addition
   (- e e) ; subtraction
   (* e e))) ; multiplication

(define-judgment-form cas
  #:contract (= e e)
  #:mode (= I O)

  [
   --- "reflexivity"
   (= e e)]

  ; Symmetry cannot be represented in judgement forms since
  ; as I can't bind e_1
  ; [(= e_1 e_2)
  ; --- "symmetry"
  ;  (= e_2 e_1)]

  ; TODO: If transitivity is on, the evaluator gets caught and OOMs
  ; Without it, I can't show that (judgment-holds (= (+ b (- a b)) a))
  ; Symmetry is needed at (+ (+ b (- b)) a) to convert (+ b (- b)) to
  ; (- b b) by "subtraction-negation"
  ; It would be kinda hellish to create "left-hand" versions of *all* of these rules
  ; [(= e_1 e_2)
  ; (= e_2 e_3)
  ; --- "transitivity"
  ; (= e_1 e_3)]

  [
   --- "addition communicative"
   (= (+ e_1 e_2) (+ e_2 e_1))]

  [
   --- "addition associative"
   (= (+ e_1 (+ e_2 e_3)) (+ (+ e_1 e_2) e_3))]

  ; Philosophical question: should this instead be a reduction rule?
  [
   --- "addition identity"
   (= (+ e 0) e)]

  [
   --- "subtraction-negation"
   (= (- e_1 e_2) (+ e_1 (~ e_2)))]

  [
   --- "subtraction anti-communicative"
   (= (- e_1 e_2) (~ (- e_2 e_1)))]

  [
   --- "subtraction inverse"
   (= (- e e) 0)]

  [
   --- "multiplication communicative"
   (= (* e_1 e_2) (* e_2 e_1))]

  [
   --- "multiplication associative"
   (= (* e_1 (* e_2 e_3)) (* (* e_1 e_2) e_3))]

  [
   --- "multiplication distributive"
   (= (* e_1 (+ e_2 e_3)) (+ (* e_1 e_2) (* e_1 e_2)))]

  [
   --- "multiplication identity"
   (= (* e 1) e)]

  [
   --- "multiplication zero"
   (= (* e 0) 0)]

  [
   --- "multiplication negation"
   (= (* e -1) (~ e))])

(define-metafunction cas
  add : number number -> number
  [(add number_1 number_2)
   ,(apply + (term (number_1 number_2)))])

(define-metafunction cas
  sub : number number -> number
  [(sub number_1 number_2)
   ,(apply - (term (number_1 number_2)))])

(define-metafunction cas
  mul : number number -> number
  [(mul number_1 number_2)
   ,(apply * (term (number_1 number_2)))])

(define-metafunction cas
  neg : number -> number
  [(neg number)
   ,(apply - (term (number)))])

; TODO: Do I *need* an eval context? (yes, you do to simplify inner expressions, dingus)
; It's at times like this I probably need to get back to reading SEwPR.
(define red
  (reduction-relation cas
    #:domain e
    (--> (+ c_1 c_2)
         (add c_1 c_2)
         "addition")

    (--> (- c_1 c_2)
         (sub c_1 c_2)
         "subtraction")

    (--> (* c_1 c_2)
         (mul c_1 c_2)
         "multiplication")

    (--> (~ c_1)
         (neg c_1)
         "negation")

    (--> e_1
         e_2
         (judgment-holds (= e_1 e_2))
         "algebra")))

; TODO: how do I add facts like a := 3?
; Probably can be done with a side condition and a few metafunctions.
