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

(defun getch->direction (getch default)
  (cond
    ((or (= getch (char-code #\w))
         (= getch (char-code #\W))
         (= getch charms/ll:KEY_UP))
     'up)

    ((or (= getch (char-code #\a))
         (= getch (char-code #\A))
         (= getch charms/ll:KEY_LEFT))
     'left)

    ((or (= getch (char-code #\s))
         (= getch (char-code #\S))
         (= getch charms/ll:KEY_DOWN))
     'down)

    ((or (= getch (char-code #\d))
         (= getch (char-code #\D))
         (= getch charms/ll:KEY_RIGHT))
     'right)

    (t default)))

(defun quit? (getch)
  (or (= getch (char-code #\q))
      (= getch (char-code #\Q))))

(defun pause? (getch)
  (= getch (char-code #\space)))

(defmacro pause-loop (quit-place)
  `(loop
      for getch = (charms/ll:getch)

      when (pause? getch)
      return nil

      when (quit? getch)
      do (setf ,quit-place t)
      and return nil))

(defun game-loop (board)
  (loop
     with quit? = nil
     with direction = 'right

     until quit?

     for getch = (charms/ll:getch)
     do (setf direction (getch->direction getch direction))

     when (or (string= direction 'up)
              (string= direction 'down))
     do (sleep (/ (/ 50 2.3) 1000))

     when (quit? getch)
     do (setf quit? t)

     when (pause? getch)
     do (pause-loop quit?)

     do (update board direction)
     when (game-over? board)
     do (setf quit? t)

     do (charms/ll:erase)
     do (draw board)
     do (charms/ll:refresh)))

(defun init-ncurses ()
  (charms/ll:initscr)
  (charms/ll:noecho)
  (charms/ll:curs-set 0)
  (charms/ll:keypad charms/ll:*stdscr* 1)

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
                       charms/ll:color_black)

  (let (max-y max-x)
    (charms/ll:getmaxyx charms/ll:*stdscr* max-y max-x)
    (list max-y max-x)))

(defun draw-game-over (board)
  (let* ((string (format nil "Game Over! Press q to exit"))
         (top-left (top-left board))
         (bottom-right (bottom-right board))
         (diff (- (x bottom-right)
                  (x top-left)))
         (middle (+ (x top-left)
                    (/ diff 2)))
         (x (floor (- middle (/ (length string) 2))))
         (y (+ (y bottom-right) 2)))
    (charms/ll:mvaddstr y x string)))

(defun main (columns rows)
  (destructuring-bind (max-y max-x) (init-ncurses)
    (let* ((top-left (point (floor (- (/ max-x 2)
                                      (/ columns 2)))
                            (floor (- (/ max-y 2)
                                      (/ rows 2)))))
           (bottom-right (point (+ (x top-left) (- columns 1))
                                (+ (y top-left) (- rows 1))))
           (board (board top-left bottom-right)))

      (charms/ll:timeout 50)
      (game-loop board)

      (charms/ll:timeout -1)
      (draw-game-over board)
      (charms/ll:refresh)
      (loop
         for getch = (charms/ll:getch)
         until (quit? getch))))
  (charms/ll:endwin))

(defun parse-args (args)
  "Parses list of strings into list of integers (columns rows)."
  (if (= (length args) 2)
      (list (parse-integer (first args)) (parse-integer (second args)))
      (list 60 18)))

(defun entry-point ()
  (apply #'main (parse-args (uiop:command-line-arguments))))
