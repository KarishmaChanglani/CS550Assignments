#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Provides functions that find the free variables of a lambda expression and perform beta reductions
;NOTE: The test suites provided do not cover all of the functions. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require rackunit)
(require rackunit/text-ui)
(require racket/trace)


(define (lambda-vars lexp) (cadr lexp))
(define (lambda-body lexp) (caddr lexp))

;;Finds the free variables of a lambda expression
(define (free-vars lexp)
  (if (lambda-exp? lexp)
      (let ((vars (lambda-vars lexp)) (body (lambda-body lexp)))
        (filter (lambda (x) (not (member x vars))) (flatten body)))
      '()))

(define (lambda-exp? exp) (and (pair? exp) (and (equal? 'lambda (car exp)) (= (length exp) 3))))

(define (beta-helper exp target lexp)
  (cond [(lambda-exp? exp) (beta-helper (lambda-body exp) target lexp)]
        [(pair? exp)(cons (beta-helper (car exp) target lexp) (beta-helper (cdr exp) target lexp))]
        [else (if (equal? exp target) lexp exp)]))

;;Performs and return a beta reduction on (lexp1 lexp2)
(define (beta-reduce lexp1 lexp2)
  (let ((vars (lambda-vars lexp1)) (body (lambda-body lexp1)))
    (if (null? vars) lexp1
        (if (null? (cdr vars))
            (beta-helper body (car vars) lexp2)
            (list 'lambda (cdr vars) (beta-helper body (car vars) lexp2))))))

;;Performs and returns a beta reduction on (lexp1 lexp2) only if a legal reduction can be performed. Returns 'fail otherwise.
(define (safe-beta-reduce lexp1 lexp2) (let ((free2 (free-vars lexp2)))
                                         (if (foldr (lambda (x y) (and x y)) #t (map (lambda (x) (not (member x free2))) (lambda-vars lexp1))) (beta-reduce lexp1 lexp2) 'fail)))

(define (reduce-exp-prev-old exp prev)
  (cond
    [(lambda-exp? exp) exp] ;Don't reduce under lambda
    [(pair? exp) (if (and (lambda-exp? (car exp)) (not (null? (cdr exp))))
                     (reduce-exp-prev (safe-beta-reduce (reduce-exp (car exp)) (cadr exp)) exp)
                     exp)]
    [(equal? exp 'fail) prev]
    [else exp]))

(define (reduce-exp-prev exp prev)
  (cond
    [(lambda-exp? exp) exp] ;Don't reduce under lambda
    [(pair? exp) (if (not (null? (cdr exp)))
                     (cond [(lambda-exp? (car exp)) (reduce-exp-prev (safe-beta-reduce (car exp) (cadr exp)) exp)]
                           [(pair? (car exp)) (safe-beta-reduce (reduce-exp (car exp)) (cadr exp))])
                     exp)]
    [(equal? exp 'fail) prev]
    [else exp]))

;;Returns the most reduced version of an expression
(define (reduce-exp exp)
  (reduce-exp-prev exp '()))

;Some convenient definitions
(define identity '(lambda (x) x))
(define freey '(lambda (x) (x y)))
(define freex '(lambda (y) (x y)))


(define-test-suite free-vars-test-suite
  (check-equal? (free-vars '(lambda () x)) '(x))
  (check-equal? (free-vars '(lambda (x) x)) '())
  (check-equal? (free-vars '(lambda (x) (x y))) '(y))
  (check-equal? (free-vars '(lambda (y) (x y))) '(x))
  (check-equal? (free-vars '(lambda (x y) (x y))) '())
  (check-equal? (free-vars '(lambda () (x y))) '(x y))
  (check-equal? (free-vars '(lambda (x z) (x (y (z w))))) '(y w))
  )
(run-tests free-vars-test-suite 'verbose)

(define-test-suite beta-reduce-test-suite
  (check-equal? (beta-reduce '(lambda  (x y) (x y)) '(lambda (a) a)) '(lambda (y) ((lambda (a) a) y)))
  (check-equal? (beta-reduce '(lambda  (x) x) '(lambda (a) a)) '(lambda (a) a))

  (check-equal? (safe-beta-reduce '(lambda  (x) x) '(lambda (a) a)) '(lambda (a) a))
  (check-equal? (safe-beta-reduce '(lambda  (x) x) '(lambda (y) x)) 'fail)
  (check-equal? (safe-beta-reduce '(lambda (x y) (x (x (y x)))) '(lambda (f n) (n (f n)))) '(lambda (y) ((lambda (f n) (n (f n))) ((lambda (f n) (n (f n))) (y (lambda (f n) (n (f n))))))))
  (check-equal? (reduce-exp '((lambda (x y) (x y)) (lambda (z) z))) '(lambda (y) ((lambda (z) z) y)))
  (check-equal? (reduce-exp '(((lambda (x y) (x y)) (lambda (z) z)) (lambda (u) u))) '(z (lambda (u) u))) ;;Logically incorrect?
  )
(run-tests beta-reduce-test-suite 'verbose)
(trace safe-beta-reduce)