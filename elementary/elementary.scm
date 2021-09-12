(import (chicken process-context))
(import vector-lib)

(define iteration-count
  (let* ((args (command-line-arguments))
         (ic-arg (if (null? args) "8" (car args))))
    (string->number ic-arg)))
(define cell-count (* 2 (+ 1 iteration-count)))

(define cells-curr (make-vector cell-count #f))
(vector-set! cells-curr (inexact->exact (floor (/ cell-count 2))) #t)
(define cells-last)

(define (draw-cells)
  (let loop ((times 0))
    (when (< times cell-count)
      (display (if (vector-ref cells-curr times) "*" " "))
      (loop (+ times 1))))
  (newline))

(define (update-cells)
  (define (xor x y)
    (and (or x y)
         (not (and x y))))
  (let loop ((times 1))
    (when (< times (- cell-count 1))
      (vector-set! cells-curr times
                   (xor (vector-ref cells-last (- times 1))
                        (vector-ref cells-last (+ times 1))))
      (loop (+ times 1)))))

(let loop ((times 0))
  (when (< times iteration-count)
    (draw-cells)
    (set! cells-last (vector-copy cells-curr))
    (update-cells)
    (loop (+ times 1))))
