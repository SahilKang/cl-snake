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

(in-package #:snake)

(defclass snake ()
  ((scales
    :initarg :scales
    :initform (error "Must supply :scales collection.")
    :documentation "Container of scales comprising snake.")
   (direction
    :initarg :direction
    :initform (error "Must supply a :direction")
    :reader direction
    :documentation "Snake direction: 'left 'right 'up 'down.")))

(defgeneric grow (snake direction))
(defgeneric move (snake direction))
(defgeneric head (snake))
(defgeneric body (snake))

(defun snake (head direction)
  "head is a scale and direction is 'left 'right 'up 'down."
  (let ((scales (make-array 1
                          :fill-pointer 1
                          :adjustable t
                          :element-type 'cartesian:scale
                          :initial-element head)))
    (make-instance 'snake :scales scales :direction direction)))

(defmethod head ((snake snake))
  (with-slots (scales) snake
    (let ((last-scale (elt scales (- (length scales) 1))))
      last-scale)))

(defmethod body ((snake snake))
  (with-slots (scales) snake
    (let ((all-but-last (subseq scales 0 (- (length scales) 1))))
      (nreverse all-but-last))))

(defmethod x ((snake snake))
  (x (head snake)))

(defmethod y ((snake snake))
  (y (head snake)))

(defun opposite-directions? (dir-1 dir-2)
  "Return true if directions are opposites."
  (let ((pair (list dir-1 dir-2)))
    (or (subsetp '(left right) pair :test #'string=)
        (subsetp '(up down) pair :test #'string=))))

(defun next-position (snake &optional (direction (direction snake)))
  "Return point of the snake's next position."
  (let ((x (x snake))
        (y (y snake)))
    (cond
      ((string= direction 'up) (point x (- y 1)))
      ((string= direction 'down) (point x (+ y 1)))
      ((string= direction 'left) (point (- x 1) y))
      ((string= direction 'right) (point (+ x 1) y))
      (t (error "Unknown direction.")))))

(defmethod grow ((snake snake) direction)
  (with-slots (scales (snake-direction direction)) snake
    (unless (opposite-directions? snake-direction direction)
      (setf snake-direction direction))
    (let ((head (change-class (next-position snake) 'cartesian:scale)))
      (vector-push-extend head scales))))

(defmethod move ((snake snake) direction)
  (grow snake direction)
  (with-slots (scales) snake
    (let ((tail (elt scales 0)))
      (setf scales (delete tail scales :test #'eq :count 1)))))
