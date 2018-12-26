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

(in-package #:draw)

(defvar *snake-body-color* 1)
(defvar *snake-head-color* 2)
(defvar *food-color* 3)
(defvar *border-color* 4)

(defvar *snake-char* #\*)
(defvar *food-char* #\o)

(defvar *pixel-char*)

(defgeneric draw (object))

(defmacro with-color (color &body body)
  (let ((c (gensym)))
    `(let ((,c (charms/ll:color-pair ,color)))
       (charms/ll:attron ,c)
       (unwind-protect
            ,@body
         (charms/ll:attroff ,c)))))

(defmethod draw ((point point))
  (charms/ll:mvaddch (y point) (x point) (char-code *pixel-char*)))

(defmethod draw ((snake snake))
  (let ((body (body snake)))
    (let ((*pixel-char* *snake-char*))
      (with-color *snake-head-color*
        (draw (elt body (- (length body) 1))))
      (with-color *snake-body-color*
        (loop for i from 0 below (- (length body) 1)
           for scale = (elt body i)
           do (draw scale))))))

(defmethod draw ((food food))
  (let ((*pixel-char* *food-char*))
    (with-color *food-color*
      (call-next-method))))

(defmethod draw ((board board))
  (let ((snake (get-snake board))
        (food (get-food board))
        (points (points board))
        (top-left (top-left board))
        (bottom-right (bottom-right board)))
    (draw snake)
    (draw food)

    ;; draw border corners
    (with-color *food-color*
      (let ((*pixel-char* #\+))
        (draw top-left)
        (draw bottom-right)
        (charms/ll:mvaddch (y bottom-right)
                           (x top-left)
                           (char-code *pixel-char*))
        (charms/ll:mvaddch (y top-left)
                           (x bottom-right)
                           (char-code *pixel-char*))))

    ;; draw border
    (with-color *border-color*
      (loop for x from (+ 1 (x top-left)) below (x bottom-right)
         for top-border = (y top-left)
         for bottom-border = (y bottom-right)
         do (charms/ll:mvaddch top-border x (char-code #\-))
         do (charms/ll:mvaddch bottom-border x (char-code #\-)))
      (loop for y from (+ 1 (y top-left)) below (y bottom-right)
         for left-border = (x top-left)
         for right-border = (x bottom-right)
         do (charms/ll:mvaddch y left-border (char-code #\|))
         do (charms/ll:mvaddch y right-border (char-code #\|))))

    ;; draw points
    (let* ((string (format nil "Points: ~A" points))
           (diff (- (x bottom-right)
                    (x top-left)))
           (middle (+ (x top-left)
                      (/ diff 2)))
           (x (floor (- middle (/ (length string) 2))))
           (y (+ (y bottom-right) 3)))
      (charms/ll:mvaddstr y x string))))
