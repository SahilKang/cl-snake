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

(in-package #:cl-user)

(defpackage #:cartesian
  (:use #:cl)
  (:export #:point
           #:scale
           #:food

           #:x #:y
           #:distance))

(defpackage #:snake
  (:use #:cl #:cartesian)
  (:export #:snake

           #:x #:y
           #:grow #:move
           #:next-position
           #:direction
           #:body))

(defpackage #:board
  (:use #:cl #:cartesian #:snake)
  (:export #:board

           #:update
           #:game-over?
           #:top-left #:bottom-right
           #:get-snake
           #:get-food
           #:points
	   #:banner))

(defpackage #:draw
  (:use #:cl #:cartesian #:snake #:board)
  (:export #:draw
           #:*snake-body-color*
           #:*snake-head-color*
           #:*food-color*
           #:*border-color*
           #:*snake-char*
           #:*food-char*))

(defpackage #:main
  (:use #:cl #:board #:draw #:cartesian)
  (:export #:entry-point))
