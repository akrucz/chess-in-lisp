;;;; Austin Kruczek
;;;; Chess in LISP
;;;; Version 1.0

;;; Main

(defun str+ (&rest rest)
   (format nil "~{~a~}" rest))

(defun make-board ()
"Creates a list of lists of ints. First list index is row. Nested lists' item indices are columns."
   (let ((main '(6 3 4 7 9 4 3 6)) (pawns '(2 2 2 2 2 2 2 2)) (zeros '(0 0 0 0 0 0 0 0)))
      (list main pawns zeros zeros zeros zeros
         (mapcar #'(lambda (x) (setf x (- 0 x))) pawns)
         (mapcar #'(lambda (x) (setf x (- 0 x))) main))))


(defconstant dirpath "~/Desktop/chess-in-lisp/")

(load (str+ dirpath "ui.lisp"))
(load (str+ dirpath "move-validation.lisp"))
(load (str+ dirpath "chess-utils.lisp"))
(load (str+ dirpath "adversary.lisp"))
(load (str+ dirpath "core-functions.lisp"))

;(play-chess :automatic-game t)
