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
  (let ((*pixel-char* *snake-char*))
    (with-color *snake-head-color*
      (draw (head snake)))
    (with-color *snake-body-color*
      (loop
	 for scale across (body snake)
	 do (draw scale)))))

(defmethod draw ((food food))
  (let ((*pixel-char* *food-char*))
    (with-color *food-color*
      (call-next-method))))

(defun draw-border-corners (top-left bottom-right)
  "Draw the border corners."
  (with-color *food-color*
    (let ((*pixel-char* #\+)
	  (bottom-left (point (x top-left) (y bottom-right)))
	  (top-right (point (x bottom-right) (y top-left))))
      (draw top-left)
      (draw bottom-right)
      (draw bottom-left)
      (draw top-right))))

(defun draw-border-edges (top-left bottom-right)
  (flet ((draw-char (x y char) (charms/ll:mvaddch y x (char-code char))))
    (with-color *border-color*

      ;; top and bottom edges
      (loop
	 with top-y = (y top-left)
	 with bottom-y = (y bottom-right)

	 for x from (+ 1 (x top-left)) below (x bottom-right)

	 do (draw-char x top-y #\-)
	 do (draw-char x bottom-y #\-))

      ;; left and right edges
      (loop
	 with left-x = (x top-left)
	 with right-x = (x bottom-right)

	 for y from (+ 1 (y top-left)) below (y bottom-right)

	 do (draw-char left-x y #\|)
	 do (draw-char right-x y #\|)))))

(defun draw-border (top-left bottom-right)
  "Draw border."
  (draw-border-corners top-left bottom-right)
  (draw-border-edges top-left bottom-right))

(defun horizontal-center (x1 x2 string)
  "Return horizontal center for string between x1 and x1."
  (let* ((mid-str (/ (length string) 2))
	 (delta (abs (- x1 x2)))
	 (mid-x (+ x1 (/ delta 2))))
    (floor (- mid-x mid-str))))

(defun draw-string (x y string)
  (charms/ll:mvaddstr y x string))

(defun draw-banner (top-left bottom-right banner)
  (let ((x (horizontal-center (x top-left) (x bottom-right) banner))
	(y (+ 2 (y bottom-right))))
    (draw-string x y banner)))

(defun draw-points (top-left bottom-right points)
  (let* ((string (format nil "Points: ~A" points))
	 (x (horizontal-center (x top-left) (x bottom-right) string))
	 (y (+ 3 (y bottom-right))))
    (draw-string x y string)))

(defmethod draw ((board board))
  (let ((snake (get-snake board))
        (food (get-food board))
        (points (points board))
	(banner (banner board))
        (top-left (top-left board))
        (bottom-right (bottom-right board)))
    (draw snake)
    (draw food)
    (draw-border top-left bottom-right)
    (draw-points top-left bottom-right points)
    (draw-banner top-left bottom-right banner)))

(defun vertical-direction? (board)
  (let ((direction (direction (get-snake board))))
    (or (string= direction 'up)
	(string= direction 'down))))

(defun shitty-framerate-hack (board)
  (when (vertical-direction? board)
    (sleep (/ (/ 50 2.3) 1000))))

(defmethod draw ((state state))
  (let ((board (get-board state)))
    (shitty-framerate-hack board)
    (draw board)))
