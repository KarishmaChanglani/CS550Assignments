#lang racket
(require racket/stream)
(require math/number-theory)
(require rackunit)

(require racket/trace)

(define (stream-enumerate-interval low high)
(if (> low high)
empty-stream
(stream-cons
low
(stream-enumerate-interval (+ low 1) high))))

(trace stream-enumerate-interval)
(stream-first
(stream-rest
 (stream-filter prime?
 (stream-enumerate-interval 10000 1000000))))


