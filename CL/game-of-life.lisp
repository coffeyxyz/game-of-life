;; --- game-of-life.lisp ---
;;
;; John Conway's: Game of Life, printed directly to the terminal.
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the MIT license

(defparameter *grid-width* 40)   ; Cell grid width in characters
(defparameter *grid-height* 24)  ; Cell grid height in characters

(defparameter *sleep-time* 0.1)  ; Number of seconds to sleep between updates

(defun draw-cells (cells)
  ;; THIS IS THE PROBLEM
  (loop for i from 0 to *grid-height* do
    (loop for j from 0 to *grid-width* do
      (print (aref cells i j)))
      ;(fresh-line)
        ))

(defun copy-cells (cells))

(defun step-cells (cells)
  (let ((snapshot (copy-cells cells)))
    (loop for row in snapshot do
      (loop for cell in row do
        (print cell)))))

(defun main ()
  (let ((cells (make-array (list *grid-height* *grid-width*)
                           :initial-element 0)))
    (loop while T do
      (draw-cells cells)
      ;(setq cells (step-cells cells))
      ;(terpri)
      (sleep *sleep-time*))))
