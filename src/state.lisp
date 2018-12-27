;;; ===========================================================================
;;; Copyright (C) 2018 Sahil Kang <sahil.kang@asilaycomputing.com>
;;;
;;; This file is part of cl-snake.
;;;
;;; cl-snake is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; cl-snake is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with cl-snake.  If not, see <http://www.gnu.org/licenses/>.
;;; ===========================================================================

(in-package #:state)

(defmacro defconst (name value)
  `(load-time-value
    (defconstant ,name ,value)))

;; banner messages
(defconst +running+ "Use arrows/WASD to move and spacebar to pause")
(defconst +paused+ "Paused! Press spacebar to resume")
(defconst +game-over+ "Game Over! Press spacebar to restart or enter to quit")

(defgeneric change (state input))

(defclass state ()
  ((running?
    :initform t
    :reader running?)
   (paused?
    :initform nil)
   (board
    :initarg :board
    :initform (error "Must supply a :board.")
    :reader get-board)))

(defun state (width height terminal-width terminal-height)
  (let* ((half-w (/ width 2))
	 (half-h (/ height 2))
	 (half-tw (/ terminal-width 2))
	 (half-th (/ terminal-height 2))

	 (top-left (point (floor (- half-tw half-w))
			  (floor (- half-th half-h))))

	 (bottom-right (point (+ (x top-left) (- width 1))
			      (+ (y top-left) (- height 1))))

	 (board (board top-left bottom-right)))
    (setf (banner board) +running+)
    (make-instance 'state :board board)))

(defgeneric restart-state (state))

(defun pause? (input)
  (= input (char-code #\space)))

(defun quit? (input)
  (or (= input (char-code #\newline))
      (= input (char-code #\return))))

(defun restart? (input)
  (= input (char-code #\space)))

(defun input->direction (input current-direction)
  (cond
    ((or (= input (char-code #\w))
         (= input (char-code #\W))
         (= input charms/ll:KEY_UP))
     'up)

    ((or (= input (char-code #\a))
         (= input (char-code #\A))
         (= input charms/ll:KEY_LEFT))
     'left)

    ((or (= input (char-code #\s))
         (= input (char-code #\S))
         (= input charms/ll:KEY_DOWN))
     'down)

    ((or (= input (char-code #\d))
         (= input (char-code #\D))
         (= input charms/ll:KEY_RIGHT))
     'right)

    (t current-direction)))

(defmethod restart-state ((state state))
  (with-slots (running? paused? board) state
    (setf running? t
	  paused? nil
	  board (board (top-left board) (bottom-right board))
	  (banner board) +running+)))

;; making a state-machine DSL might be fun
(defmethod change ((state state) (input integer))
  (with-slots (running? paused? board) state
    (if (not running?)
	(error "Reached terminal state!")
	(cond
	  ((game-over? board)
	   (cond
	     ((quit? input) (setf running? nil))
	     ((restart? input) (restart-state state))))

	  (paused?
	   (when (pause? input)
	     (setf paused? nil
		   (banner board) +running+)))

	  ((pause? input)
	   (setf paused? t
		 (banner board) +paused+))

	  (t
	   (let* ((current-direction (direction (get-snake board)))
		  (new-direction (input->direction input current-direction)))
	     (update board new-direction)
	     (when (game-over? board)
	       (setf (banner board) +game-over+))))))))
