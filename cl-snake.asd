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

(asdf:defsystem #:cl-snake
  :description "Play snake in the terminal."
  :version "1.0.0"
  :author "Sahil Kang <sahil.kang@asilaycomputing.com>"
  :license "GPLv3"
  :depends-on (#:cl-charms)
  :entry-point "main:entry-point"
  :build-pathname "cl-snake"
  :components
  ((:module
    "src"
    :serial t
    :components
    ((:file "package")
     (:file "cartesian")
     (:file "snake")
     (:file "board")
     (:file "state")
     (:file "draw")
     (:file "main")))))

#+sb-core-compression
(defmethod asdf:perform ((op asdf:image-op) (sys asdf:system))
  (uiop:dump-image (asdf:output-file op sys) :executable t :compression 9))
