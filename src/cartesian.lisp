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

(in-package #:cartesian)

(defclass point ()
  ((x
    :initarg :x
    :initform (error "Must supply :x")
    :reader x
    :documentation "x coordinate")
   (y
    :initarg :y
    :initform (error "Must supply :y")
    :reader y
    :documentation "y coordinate")))

(defun point (x y)
  (make-instance 'point :x x :y y))

(defgeneric distance (p1 p2))

(defun euclidean-distance (delta-x delta-y)
  (isqrt (+ (expt delta-x 2) (expt delta-y 2))))

(defmethod distance ((p1 point) (p2 point))
  "Calculate euclidean distance.
The two other values are signed horizontal and vertical distance."
  (let* ((horiz (- (x p1) (x p2)))
         (vert (- (y p1) (y p2)))
         (dist (euclidean-distance horiz vert)))
    (values dist horiz vert)))

(defclass scale (point)
  ())

(defun scale (x y)
  (make-instance 'scale :x x :y y))

(defclass food (point)
  ())

(defun food (x y)
  (make-instance 'food :x x :y y))
