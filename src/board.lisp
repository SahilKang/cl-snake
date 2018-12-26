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

(in-package #:board)

(defclass board ()
  ((top-left
    :initarg :top-left
    :initform (error "Must supply :top-left")
    :reader top-left
    :documentation "Pixel representing top-left of board.")
   (bottom-right
    :initarg :bottom-right
    :initform (error "Must supply :bottom-right")
    :reader bottom-right
    :documentation "Pixel representing bottom-right of board.")
   (snake
    :initarg :snake
    :initform (error "Must supply :snake")
    :reader get-snake
    :documentation "Snake inside game board.")
   (food
    :initarg :food
    :initform (error "Must supply :food.")
    :reader get-food
    :documentation "Snake food inside game board.")
   (points
    :initform 0
    :reader points
    :documentation "Current points in game.")
   (banner
    :initform "Use arrows or WASD, spacebar to pause, and q to quit."
    :accessor banner
    :documentation "Message to display for game board.")))

(defgeneric update (board direction))
(defgeneric game-over? (board))

(defun shit-dimensions?
    (top-left bottom-right &optional (min-distance 10))
  "Return true if rectangle does not have big-enough dimensions."
  (multiple-value-bind (_ horiz vert) (distance top-left bottom-right)
    (declare (ignore _))
    (< (min (abs horiz) (abs vert)) min-distance)))

(defun board (top-left bottom-right)
  (when (shit-dimensions? top-left bottom-right)
    (error "Dimensions are shit."))
  (let* ((head (scale (+ 1 (x top-left)) (+ 2 (y top-left))))
         (snake (snake head 'right))
         (food (food (+ 3 (x snake)) (y snake))))
    (loop
       repeat 15
       do (grow snake 'right))
    (make-instance 'board
                   :top-left top-left
                   :bottom-right bottom-right
                   :snake snake
                   :food food)))

(defgeneric gen-food (board))
(defgeneric snake-gonna-eat? (board direction))

(defun random-range (start end)
  "Random number in exclusive range (start, end)."
  (+ 1 start (random (- end start 1))))

(defun random-food (top-left bottom-right)
  (let ((x (random-range (x top-left) (x bottom-right)))
        (y (random-range (y top-left) (y bottom-right))))
    (food x y)))

(defmethod gen-food ((board board))
  (with-slots (food top-left bottom-right) board
    (setf food (random-food top-left bottom-right))))

(defmethod snake-gonna-eat? ((board board) direction)
  "Determine if the snake is going to eat the food in the next update."
  (with-slots (snake food) board
    (= 0 (distance (next-position snake direction) food))))

(defmethod update ((board board) direction)
  (with-slots (snake points) board
    (if (snake-gonna-eat? board direction)
        (progn
          (grow snake direction)
          (gen-food board)
          (incf points))
        (move snake direction))))

(defgeneric snake-out-of-bounds? (board))

(defmethod snake-out-of-bounds? ((board board))
  (with-slots (snake top-left bottom-right) board
    (let ((x (x snake))
          (y (y snake)))
      (or (<= x (x top-left))
          (>= x (x bottom-right))
          (<= y (y top-left))
          (>= y (y bottom-right))))))

(defun snake-overlapped? (snake)
  (let ((body (body snake)))
    (let ((head (elt body (- (length body) 1)))
          (same-position? (lambda (scale-1 scale-2)
                            (= 0 (distance scale-1 scale-2)))))
      (loop for i from 0 below (- (length body) 1)
         for body-scale = (elt body i)
         when (funcall same-position? head body-scale)
         do (return-from snake-overlapped? t))
      nil)))

(defmethod game-over? ((board board))
  (with-slots (snake) board
    (or (snake-out-of-bounds? board)
        (snake-overlapped? snake))))
