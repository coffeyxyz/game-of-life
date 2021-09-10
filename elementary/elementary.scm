(import (chicken process-context))
(import vector-lib)

(define n
  (let* ((args (command-line-arguments))
         (n-arg (if (null? args) "8" (car args))))
    (string->number n-arg)))
(define count (* 2 n))

(define cells-curr (make-vector count #f))
(vector-set! cells-curr (inexact->exact (floor (/ count 2))) #t)
(define cells-last)

(define (draw-cells)
  (let loop ((times 0))
    (when (< times count)
      (display (if (vector-ref cells-curr times) "*" " "))
      (loop (+ times 1))))
  (newline))

(define (update-cells)
  (define (xor x y)
    (and (or x y)
         (not (and x y))))
  (let loop ((times 1))
    (when (< times (- count 1))
      (vector-set! cells-curr times
                   (xor (vector-ref cells-last (- times 1))
                        (vector-ref cells-last (+ times 1))))
      (loop (+ times 1)))))

(let loop ((times 0))
  (when (< times (- n 1))
    (draw-cells)
    (set! cells-last (vector-copy cells-curr))
    (update-cells)
    (loop (+ times 1))))
