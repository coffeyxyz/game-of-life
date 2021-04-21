#!/usr/bin/sbcl --script

;; --- game-of-life.lisp ---
;;
;; John Conway's: Game of Life, printed directly to the terminal.
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the MIT license

(defparameter *grid-width* 40)   ; Cell grid width in characters
(defparameter *grid-height* 24)  ; Cell grid height in characters

(defparameter *sleep-time* 1.0)  ; Number of seconds to sleep between updates

(defun draw-cells (cells)
  (terpri)
  (loop for i from 0 below *grid-height* do
    (loop for j from 0 below *grid-width* do
      (prin1 (aref cells i j)))
    (fresh-line)))

(defun copy-cells (cells))

(defun step-cells (cells)
  (let ((snapshot (copy-cells cells)))
    (loop for cell in cells do
      (print cell))))

(defun main ()
  (let ((cells (make-array (list *grid-height* *grid-width*)
                           :initial-element 0)))
    (loop while T do
      (draw-cells cells)
      ;(setq cells (step-cells cells))
      (sleep *sleep-time*))))
(main)
