#lang racket
(require redex)

(define-language λv (e (e e ...) (if0 e e e) x v) (v (λ (x ...) e) number +) (E (v ... E e ...) (if0 E e e) hole) (x (variable-except λ + if0)))
(define red (reduction-relation λv (--> (in-hole E (+ number_1 number_2)) (in-hole E ,(+ (term number_1) (term number_2))) "+") (--> (in-hole E (if0 0 e_1 e_2)) (in-hole E e_1) "if0t") (--> (in-hole E (if0 number_1 e_1 e_2)) (in-hole E e_2) "if0f" (side-condition (not (= 0 (term number_1))))) (--> (in-hole E ((λ (x ..._1) e) v ..._1)) (in-hole E (subst-n (x v) ... e)) "βv")))
(define-metafunction λv subst-n : (x any) ... any -> any [(subst-n (x_1 any_1) (x_2 any_2) ... any_3) (subst x_1 any_1 (subst-n (x_2 any_2) ... any_3))] [(subst-n any_3) any_3])
(define-metafunction λv subst : x any any -> any   ;; 1. x_1 bound, so don't continue in λ body
  [(subst x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2)) (λ (x_2 ... x_1 x_3 ...) any_2) (side-condition (not (member (term x_1) (term (x_2 ...)))))]   ;; 2. general purpose capture avoiding case
  [(subst x_1 any_1 (λ (x_2 ...) any_2)) (λ (x_new ...) (subst x_1 any_1 (subst-vars (x_2 x_new) ... any_2))) (where (x_new ...) ,(variables-not-in (term (x_1 any_1 any_2)) (term (x_2 ...))))]   ;; 3. replace x_1 with e_1
  [(subst x_1 any_1 x_1) any_1]   ;; 4. x_1 and x_2 are different, so don't replace
  [(subst x_1 any_1 x_2) x_2]   ;; the last cases cover all other expressions
  [(subst x_1 any_1 (any_2 ...)) ((subst x_1 any_1 any_2) ...)] [(subst x_1 any_1 any_2) any_2])
(define-metafunction λv subst-vars : (x any) ... any -> any [(subst-vars (x_1 any_1) x_1) any_1] [(subst-vars (x_1 any_1) (any_2 ...)) ((subst-vars (x_1 any_1) any_2) ...)] [(subst-vars (x_1 any_1) any_2) any_2] [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3) (subst-vars (x_1 any_1) (subst-vars (x_2 any_2) ... any_3))] [(subst-vars any) any])
(traces red (term ((λ (n) (if0 n 1 ((λ (x) (x x)) (λ (x) (x x))))) (+ 2 2))))

(define value? (redex-match λv v))
(define (single-step? e) (= (length (apply-reduction-relation red e)) 1))
(redex-check λv e (or (value? (term e)) (single-step? (term e))))


(traces red (term (((λ (x) (λ (y) (x y))) (λ (n) (+ n n))) 5))) ;Should be 10
(traces red (term ((λ (x) (λ (y) (x y))) ((λ (n) (+ n n)) 5)))) ;Should reduce to lambda y. (10 y)
(traces red (term ((((λ (x) (λ (y) (x y))) (λ (z) (+ 2 z))) (λ (u) u)) 5))) ;A mess
