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

;; Create a new array of cells
(defun create-cells ()
  (make-array (list *grid-height* *grid-width*)
              :initial-element 0))

;; Print an array of cells
(defun draw-cells (cells)
  (terpri)
  (loop for y from 0 below *grid-height* do
    (loop for x from 0 below *grid-width* do
      (prin1 (aref cells y x)))
    (fresh-line)))

;; Create a copy of an array of cells
(defun copy-cells (cells)
  (let ((copy (create-cells)))
    (loop for y from 0 below *grid-height* do
      (loop for x from 0 below *grid-width* do
        (setf (aref copy y x)
              (aref cells y x))))
    copy))

;; Get the number of living neighbors of a given cell
(defun get-neighbor-count (cells y x)
  3)

;; Should a given cell live
(defun should-live (cells y x)
  (let ((is-alive (aref cells y x))
        (neighbor-count (get-neighbor-count cells y x)))
    (if (= is-alive 1)
        (cond ((< neighbor-count 2) 0)
              ((> neighbor-count 3) 0)
              (t 1))
        (cond ((= neighbor-count 3) 1)
              (t 0)))))

;; Update an array of cells
(defun update-cells (cells)
  (let ((snapshot (copy-cells cells)))
    (loop for y from 0 below *grid-height* do
      (loop for x from 0 below *grid-width* do
        (setf (aref cells y x)
              (should-live snapshot y x))))
    cells))

(defun main ()
  (let ((cells (create-cells)))
    (loop while T do
      (draw-cells cells)
      (setf cells (update-cells cells))
      (sleep *sleep-time*))))
(main)
