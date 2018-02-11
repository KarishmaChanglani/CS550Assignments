#lang racket
(require racket/stream)
(require math/number-theory)
(require rackunit)
(require rackunit/text-ui)
(require racket/trace)

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons low (stream-enumerate-interval (+ low 1) high))))


;Checks
(define-test-suite stream-enumerate-interval-test-suite
  ;Bounds check
  (check-equal? (stream-enumerate-interval 10 1) empty-stream "pos low > pos high is empty-stream")
  (check-equal? (stream-enumerate-interval -1 -10) empty-stream "neg low > neg high is empty-stream")
  (check-equal? (stream-enumerate-interval 10 -10) empty-stream "neg low > neg high is empty-stream")

  ;Single element check
  (check-equal? (let ((teststream (stream-enumerate-interval 10 10))) (and (= 10 (stream-first teststream)) (stream-empty? (stream-rest teststream)))) #t  "low = high generates only low")

  ;Correctness check
  (check-equal? (stream->list (stream-enumerate-interval 0 10)) (list 0 1 2 3 4 5 6 7 8 9 10) "[0,10] generates correct output")
  (check-equal? (stream->list (stream-enumerate-interval -10 0)) (list -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0) "[-10,0] generates correct output")
  )
(run-tests stream-enumerate-interval-test-suite 'verbose)

(trace stream-enumerate-interval)
(define-test-suite part-1-test-suite
  (check-equal? (stream-first
                 (stream-rest
                  (stream-filter prime?
                                 (stream-enumerate-interval 10000 1000000)))) 10009 "The second prime after 10000 is 10009")
  ; N. J. A. Sloane, "Table of n, prime(n) for n = 1..100000", Online Encyclopedia of Integer Sequences, 2018. [Online]. Available: http://oeis.org/A000040/a000040.txt.  [Accessed: Feb 10, 2018].
  )
(run-tests part-1-test-suite 'verbose)