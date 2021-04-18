;; --- game-of-life.lisp ---
;;
;; John Conway's: Game of Life, printed directly to the terminal.
;;
;; Copyright (C) 2021 Robert Coffey
;; Released under the MIT license

(defparameter *sw* 40)  ; Screen width
(defparameter *sh* 24)  ; Screen height

(defparameter *cells*
  (make-list *sh*
             :initial-element (make-list *sw*
                                         :initial-element 0)))
(defparameter *snapshot* '())

(defun draw-cells (cells)
  (loop for i in cells do
    (loop for j in i do
      (princ j))
      (fresh-line)))

(defun main ()
  (draw-cells *cells*))
