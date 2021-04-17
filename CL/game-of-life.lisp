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

(defun main ())
