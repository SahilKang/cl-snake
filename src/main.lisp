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

(in-package #:main)

(defun init-colors ()
  (charms/ll:start-color)

  (charms/ll:init-pair *snake-body-color*
                       charms/ll:color_cyan
                       charms/ll:color_cyan)

  (charms/ll:init-pair *snake-head-color*
                       charms/ll:color_white
                       charms/ll:color_white)

  (charms/ll:init-pair *food-color*
                       charms/ll:color_red
                       charms/ll:color_black)

  (charms/ll:init-pair *border-color*
                       charms/ll:color_green
                       charms/ll:color_black))

(defun init-ncurses ()
  (charms/ll:initscr)
  (charms/ll:noecho)
  (charms/ll:curs-set 0)
  (charms/ll:keypad charms/ll:*stdscr* 1)
  (charms/ll:timeout 50)

  (init-colors)

  (let (terminal-width terminal-height)
    (charms/ll:getmaxyx charms/ll:*stdscr* terminal-height terminal-width)
    (list terminal-width terminal-height)))

(defun game-loop (state)
  (loop
     while (running? state)

     for input = (charms/ll:getch)
     do (change state input)

     do (charms/ll:erase)
     do (draw state)
     do (charms/ll:refresh)))

(defun main (width height)
  (destructuring-bind (terminal-width terminal-height) (init-ncurses)
    (let ((state (state width height terminal-width terminal-height)))
      (game-loop state)))
  (charms/ll:endwin))

(defun parse-args (args)
  "Parses list of strings into list of integers (width height)."
  (if (= (length args) 2)
      (list (parse-integer (first args)) (parse-integer (second args)))
      (list 60 18)))

(defun entry-point ()
  (apply #'main (parse-args (uiop:command-line-arguments))))
